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
  if (document.readyState == "complete") {
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
    
    httpRequest.onreadystatechange = handleResponse;
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
    interval_id = window.setInterval(interval_callback, 1000);
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

  function handleResponse() {
    console.log("In handleResponse, readyState: " + httpRequest.readyState + " status: " + httpRequest.status);
    if (httpRequest.readyState == 4 && httpRequest.status == 200) {                                   // SUCCESS
      //  httpRequest.overrideMimeType("application/json");
      //document.getElementsByClassName("lds-spinner")[0].setAttribute("hidden", "hidden");
      document.getElementsByClassName("lds-spinner")[0].style.display = "none";
      clearInterval(interval_id);  // stop the timer
      document.getElementById("finish-time-container").style.display = "block";
      document.getElementById("finish-time").innerHTML = new Date().toString();//.substr(0, 19);
      var data = JSON.parse(httpRequest.responseText);

      if ("error_messages" in data && data.error_messages.length > 0) {
        el = document.getElementById("error_messages");
        for (i=0; i < data.error_messages.length; ++i) {
          el.innerHTML += "<p>" + data.error_messages[i] + "</p>";
        }
        document.getElementById("submit").disabled = false;
        return;
      }
      document.getElementById("info").innerHTML += data.result[0];
      document.getElementById("table_container").innerHTML = data.summary_table[0];
      
      if (!document.getElementById("random-forest").checked) {
        document.getElementById("variable-importance").style.display = "none";
        //document.getElementById("detail-rf").style.display = "none";
      }
      if (!document.getElementById("lmm").checked && !document.getElementById("dlmm").checked) {
        document.getElementById("effect-size").style.display = "none";
        //document.getElementById("detail-lmm-male").style.display = "none";
        //document.getElementById("detail-lmm-female").style.display = "none";
      }
      
      // Add pointers to separate result pages in the details table
      console.log(`Details dataframe has ${data.details_df.length} rows`);
      detailed_results = document.getElementById("detailed-results").firstElementChild;  // get the tbody element under the table element
      for (i=0; i < data.details_df.length; ++i) {
        var wrapper= document.createElement('tbody');
        e = data.details_df[i];
        t = `<tr id="${e.id}"> <td>${e.pretty}</td> <td>${e.gender}</td> <td><a href="${e.html}" target="_blank" >html</a></td> <td><a href="${e.pdf}" target="_blank" >pdf</a></td> </tr>`;
        wrapper.innerHTML = t;
        console.log(t)
        detailed_results.appendChild(wrapper.firstChild);
      }
      
      document.getElementById("results-container").removeAttribute("hidden");  
      
      if (document.querySelector('input[name="input_format"]:checked').value == "Preprocessed")
        document.getElementById("preprocessed").style.display = "none";
    } else if (httpRequest.readyState == 4 && httpRequest.status != 200) {                            // FAIL
      console.log("Server error! readyState: " + httpRequest.readyState + " status: " + httpRequest.status);
      document.getElementsByClassName("lds-spinner")[0].style.display = "none";
      clearInterval(interval_id);  // stop the timer
      el = document.getElementById("error_messages");
      el.innerHTML = "<p>Server error!  readyState: " + httpRequest.readyState + " status: " + httpRequest.status + "</p>  ";
    }
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