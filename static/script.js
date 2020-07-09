document.onreadystatechange = function() {
  console.log("Executing Javascript");
  if (document.readyState == "complete") {
    console.log("Setting handleButtonPress");
    document.getElementById("submit").onclick = handleButtonPress;
  }

  var httpRequest;
  var start_time;
  var time=document.getElementById("time");
  var interval_id;
  
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
    httpRequest.open("POST", form.action);
    httpRequest.setRequestHeader("Accept", "application/json");
    httpRequest.send(formData);
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
  
  function handleResponse() {
    console.log("In handleResponse");
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
      document.getElementById("table_container").innerHTML = data.errors[0];
      
      
      document.getElementById("results-container").removeAttribute("hidden");  
    } else if (httpRequest.readyState == 4 && httpRequest.status != 200) {
      console.log("Server error!");
      document.getElementsByClassName("lds-spinner")[0].style.display = "none";
      clearInterval(interval_id);  // stop the timer
      el = document.getElementById("error_messages");
      el.innerHTML = "<p>Server error!</p>  ";
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