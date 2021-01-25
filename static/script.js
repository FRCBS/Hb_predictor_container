document.onreadystatechange = function() {
  console.log("Executing Javascript");
  if (document.readyState == "complete") {
    console.log("Setting handleButtonPress");
    document.getElementById("submit").onclick = handleButtonPress;
    document.getElementById("FRCBS").onchange = handle_input_format;
    document.getElementById("Sanquin").onchange = handle_input_format;
    document.getElementById("Preprocessed").onchange = handle_input_format;
  }

  var httpRequest;
  var start_time;
  var time=document.getElementById("time");
  var interval_id;
  
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
    if (httpRequest.readyState == 4 && httpRequest.status == 200) {
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
        document.getElementById("detail-rf").style.display = "none";
      }
      if (!document.getElementById("no-fix").checked && !document.getElementById("icp-fix").checked) {
        document.getElementById("effect-size").style.display = "none";
        document.getElementById("detail-lmm-male").style.display = "none";
        document.getElementById("detail-lmm-female").style.display = "none";
      }
      
      document.getElementById("results-container").removeAttribute("hidden");  
      
      if (document.querySelector('input[name="input_format"]:checked').value == "Preprocessed")
        document.getElementById("preprocessed").style.display = "none";
    } else if (httpRequest.readyState == 4 && httpRequest.status != 200) {
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