var old_unit = "gperl";

function convert_hb_unit(from, to, hb) {
    hb = parseFloat(hb);
    console.log(`In convert_hb_unit: from="${from}" to="${to}"" hb="${hb}"`);
    if (from=="gperl" && to=="gperdl") {
	return(hb/10.0);
    }  else if (from=="gperl" && to == "mmolperl") {
	return(hb * 0.01551 * 4);
    }  else if (from=="gperdl" && to == "gperl") {
	return(hb*10.0);
    }  else if (from=="gperdl" && to == "mmolperl") {
	return(hb * 10.0 * 0.01551 * 4);
    }  else if (from=="mmolperl" && to == "gperl") {
	return(hb / (0.01551 * 4));
    }  else if (from=="mmolperl" && to == "gperdl") {
	return(hb / (0.01551 * 4) / 10.0);
    } else {
	console.log("Unsupported units");
    }
}

function handle_hb_unit(e) {
    v = e.srcElement.value;
    console.log("Hb unit changed from " + old_unit + " to " + v);
    em = document.getElementById("Hb_cutoff_male");
    ef = document.getElementById("Hb_cutoff_female");
    em.value = convert_hb_unit(old_unit, v, em.value);
    ef.value = convert_hb_unit(old_unit, v, ef.value);
    old_unit = v;
}

function handle_input_format(e) {
    value = document.querySelector('input[name="input_format"]:checked').value;
    e1 = document.getElementById("donations_row");
    e2 = document.getElementById("donors_row");
    e3 = document.getElementById("donor_specific_row");
    e4 = document.getElementById("preprocessed_row");
    e5 = document.getElementById("max_diff_date_first_donation_row");
    
    if (value == "FRCBS") {
	e1.style.display = "table-row";
	e2.style.display = "table-row";
	e3.style.display = "table-row";
	e4.style.display = "none";
	e5.style.display = "none";
    } else if (value == "Sanquin") {
	e1.style.display = "table-row";
	e2.style.display = "table-row";
	e3.style.display = "none";
	e4.style.display = "none";
	e5.style.display = "table-row";
    } else {
	e1.style.display = "none";
	e2.style.display = "none";
	e3.style.display = "none";
	e4.style.display = "table-row";
	e5.style.display = "none";
    }
    
    console.log("Fieldset clicked: " + value);
}

document.onreadystatechange = function() {
    console.log("Executing Javascript");
    

    
    //<label for="lmm">
    //<input type="checkbox" value="on", id="lmm" name="lmm" />
    //Linear mixed model
    //</label>
    if (document.readyState == "complete") {
	
	// Predictive variables
	dvs = ["days_to_previous_fb", "age", "previous_Hb_def", "year",                 
	       "warm_season", "consecutive_deferrals", "recent_donations", "recent_deferrals",     
	       "hour", "previous_Hb", "Hb_first", "sex"]  ;
	el = document.getElementById("predictive-variables");
	for (i=0; i < dvs.length; ++i) {
	    l = document.createElement('label');
	    l.setAttribute("for", dvs[i]);
	    inp = document.createElement('input');
	    inp.setAttribute("type", "checkbox");
	    inp.setAttribute("value", "on");
	    inp.checked = true;
	    inp.setAttribute("id", "id_"+dvs[i]);
	    inp.setAttribute("name", "dv_"+dvs[i]);
	    l.appendChild(inp);
	    l.appendChild(document.createTextNode(dvs[i]));
	    el.appendChild(l);
	}
	
	console.log("Setting handleButtonPress");
	document.getElementById("submit").onclick = handleButtonPress;
	document.getElementById("FRCBS").onchange = handle_input_format;
	document.getElementById("Sanquin").onchange = handle_input_format;
	document.getElementById("Preprocessed").onchange = handle_input_format;
	document.getElementById("unit").onchange = handle_hb_unit;
	el = document.getElementById("unit");
	el.value="gperdl"; // Because Sanquin is the default input format, set this to its unit
	el.dispatchEvent(new Event('change', { 'bubbles': true }));  // trigger the change event
    }

    var httpRequest;
    var start_time;
    var time=document.getElementById("time");
    var interval_id;
    

    
    // This handles button press on the submit button
    function handleButtonPress(e) {
	console.log("In handleButtonPress");
	e.preventDefault();
	var form = document.getElementById("form");
	var progress = document.getElementById("progress");
	var formData = new FormData(form);
	httpRequest = new XMLHttpRequest();

	/*
	  var upload = httpRequest.upload;
	  upload.onprogress = function(e) {
	  progress.max = e.total;
	  progress.value = e.loaded;
	  };
	  upload.onload = function(e) {
	  progress.max = 1;
	  progress.value = 1;
	  };
	*/


	var el = document.getElementById("error_messages");
	el.innerHTML="";
	var el = document.getElementById("warning_messages");
	el.innerHTML="";
	var el = document.getElementById("info");
	el.innerHTML="";
	document.getElementById("results-container").hidden = true;
	
	httpRequest.onreadystatechange = handleResponseForUpload;
	//httpRequest.onreadystatechange = handleResponse;
	//httpRequest.timeout = 5000;
	httpRequest.ontimeout = handleTimeout;
	httpRequest.onerror = handleError;
	httpRequest.onabort = handleAbort;
	httpRequest.open("POST", form.action);
	httpRequest.setRequestHeader("Accept", "application/json");
	httpRequest.send(formData);
	console.log("Timeout is " + httpRequest.timeout);
	document.getElementById("submit").disabled = true;
	document.getElementById("finish-time-container").style.display = "none";
	//document.getElementsByClassName("lds-spinner")[0].removeAttribute("hidden");
	document.getElementById("error_messages").innerHTML = "";
	document.getElementsByClassName("lds-spinner")[0].style.display = "inline-block";
	
	start_time=Date.now();
	set_time(0);
	interval_id = window.setInterval(interval_callback, 1000);  // Once a second
	document.getElementById("start-time").innerHTML = new Date().toString();//.substr(0, 19);
	document.getElementById("info-container").removeAttribute("hidden");
	
    }
    
    function handleTimeout() {
	console.log("Timeout");
	el = document.getElementById("error_messages");
	el.innerHTML = "<p>Timeout</p>  ";
    }
    
    function handleError() {
	console.log("Error");
    }

    function handleAbort() {
	console.log("Abort");
    }

    function handleResponseForUpload() {
	console.log("In handleResponseForUpload, readyState: " + httpRequest.readyState + " status: " + httpRequest.status);
	if (httpRequest.readyState == 4 && httpRequest.status == 200) {                                   // SUCCESS
	    //  httpRequest.overrideMimeType("application/json");
	    //document.getElementsByClassName("lds-spinner")[0].setAttribute("hidden", "hidden");
	    //document.getElementsByClassName("lds-spinner")[0].style.display = "none";
	    //clearInterval(interval_id);  // stop the timer
	    var data = JSON.parse(httpRequest.responseText);

	    if ("error_messages" in data && data.error_messages.length > 0) {
		el = document.getElementById("error_messages");
		for (i=0; i < data.error_messages.length; ++i) {
		    el.innerHTML += "<p>" + data.error_messages[i] + "</p>";
		}
		document.getElementById("submit").disabled = false;
		stop_waiting(interval_id);
		document.getElementById("finish-time-container").style.display = "block";
		document.getElementById("finish-time").innerHTML = new Date().toString();//.substr(0, 19);

		return;
	    }
	    var exampleSocket = new WebSocket("ws://127.0.0.1:8080/");
	    exampleSocket.onmessage = function (event) {
		parsed = JSON.parse(event.data);
		process_json_result(parsed);
		console.log("Got message from server of type " + parsed.type);
		//divi.innerHTML += p("Received: " + event.data);
	    }
	    exampleSocket.onopen = function (event) {
		console.log("Websocket opened");
		//divi.innerHTML += p("Websocket opened");
		exampleSocket.send("start");
	    }
	    exampleSocket.onclose = function (event) {
		console.log("Websocket closed");
		//divi.innerHTML += p("Websocket closed");
	    }
	    
	} else if (httpRequest.readyState == 4 && httpRequest.status != 200) {                            // FAIL
	    console.log("Server error! readyState: " + httpRequest.readyState + " status: " + httpRequest.status);
	    stop_waiting(interval_id);
	    //document.getElementsByClassName("lds-spinner")[0].style.display = "none";
	    //clearInterval(interval_id);  // stop the timer
	    el = document.getElementById("error_messages");
	    el.innerHTML = "<p>Server error!  readyState: " + httpRequest.readyState + " status: " + httpRequest.status + "</p>  ";
	}
    }

    function add_rows_to_details_table(data) {
	// Add pointers to separate result pages in the details table
	console.log(`Details dataframe has ${data.details_df.length} rows`);
	// get the tbody element under the table element
	detailed_results = document.getElementById("detailed-results").firstElementChild;  
	for (i=0; i < data.details_df.length; ++i) {
            var wrapper= document.createElement('tbody');
            e = data.details_df[i];
            t = `<tr id="${e.id}"> <td>${e.pretty}</td> <td>${e.sex}</td> <td><a href="${e.html}" target="_blank" >html</a></td> <td><a href="${e.pdf}" target="_blank" >pdf</a></td> </tr>`;
            wrapper.innerHTML = t;
            console.log(t)
            detailed_results.appendChild(wrapper.firstChild);
	}
    }


    function add_error_messages(data) {
	if ("error_messages" in data && data.error_messages.length > 0) {
            el = document.getElementById("error_messages");
	    if (typeof(data.error_messages) == "string")
		data.error_messages = [data.error_messages];
            for (i=0; i < data.error_messages.length; ++i) {
		el.innerHTML += "<p>" + data.error_messages[i] + "</p>";
            }
            document.getElementById("submit").disabled = false;
            return;
	}
	
    }

    function add_warning_messages(data) {
	if ("warning_messages" in data && data.warning_messages.length > 0) {
            el = document.getElementById("warning_messages");
            if (typeof(data.warning_messages) == "string")
		data.warning_messages = [data.warning_messages];
	    for (i=0; i < data.warning_messages.length; ++i) {
		el.innerHTML += "<p>" + data.warning_messages[i] + "</p>";
            }
            document.getElementById("submit").disabled = false;
            return;
	}
	
    }

    function process_json_result(data) {
	
	if (data.type == "final") {
            //  httpRequest.overrideMimeType("application/json");
	    //document.getElementsByClassName("lds-spinner")[0].setAttribute("hidden", "hidden");
	    //document.getElementsByClassName("lds-spinner")[0].style.display = "none";
	    //clearInterval(interval_id);  // stop the timer
	    stop_waiting(interval_id);
	    document.getElementById("finish-time-container").style.display = "block";
	    document.getElementById("finish-time").innerHTML = new Date().toString();//.substr(0, 19);
	    

	    console.log("Type of data is " + typeof(data))
	    add_error_messages(data)
	    add_warning_messages(data)
	    
	    
	    if (!document.getElementById("random-forest").checked) {
		document.getElementById("variable-importance").style.display = "none";
		//document.getElementById("detail-rf").style.display = "none";
	    }
	    if (!document.getElementById("lmm").checked && !document.getElementById("dlmm").checked) {
		document.getElementById("effect-size").style.display = "none";
		//document.getElementById("detail-lmm-male").style.display = "none";
		//document.getElementById("detail-lmm-female").style.display = "none";
	    }
	    
	    //add_rows_to_details_table(data);
	    
	    //document.getElementById("results-container").removeAttribute("hidden");  
	    
	    if (document.querySelector('input[name="input_format"]:checked').value == "Preprocessed")
		document.getElementById("preprocessed").style.display = "none";
	} else if (data.type == "info") {
            // Show information about input and preprocessed dataframes
	    document.getElementById("info").innerHTML += data.result;
	} else if (data.type == "summary") {
	    // Show summary table
	    document.getElementById("table_container").innerHTML = data.summary_table_string;
	} else if (data.type == "error") {
	    // Show error
	    add_error_messages(data)
	    add_warning_messages(data)
	} else if (data.type == "warning") {
	    // Show error
	    add_error_messages(data)
	    add_warning_messages(data)
	} else if (data.type == "detail") {
	    add_rows_to_details_table(data);
	    document.getElementById("results-container").removeAttribute("hidden");  
	}
    }
    
    function handleResponse() {
	console.log("In handleResponse, readyState: " + httpRequest.readyState + " status: " + httpRequest.status);
	if (httpRequest.readyState == 4 && httpRequest.status == 200) {                                   // SUCCESS
	    var data = JSON.parse(httpRequest.responseText);
	    process_json_result(data);
	} else if (httpRequest.readyState == 4 && httpRequest.status != 200) {                            // FAIL
	    console.log("Server error! readyState: " + httpRequest.readyState + " status: " + httpRequest.status);
	    //document.getElementsByClassName("lds-spinner")[0].style.display = "none";
	    //clearInterval(interval_id);  // stop the timer
	    stop_waiting(interval_id);
	    el = document.getElementById("error_messages");
	    el.innerHTML = "<p>Server error!  readyState: " + httpRequest.readyState + " status: " + httpRequest.status + "</p>  ";
	}
    }
    
    function stop_waiting(interval_id) {
	document.getElementsByClassName("lds-spinner")[0].style.display = "none";
	clearInterval(interval_id);  // stop the timer
    }
    
    function set_time(milliseconds) {
	date = new Date(milliseconds).toISOString().substr(11, 8);
	time.innerHTML = date;
    }
    
    function interval_callback(e) {
	milliseconds = Date.now() - start_time;
	set_time(milliseconds);
    }
};  
