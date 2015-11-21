submit_page = function(type){
	var radios, mode, input, results, filename, send_info, mode_selected = false;
	
	radios = document.getElementsByName("mode_radio");
	results = document.getElementById("results");
	input = document.getElementById("writebox").value;
	filename = document.getElementById("filename").value;
	
	if(input === "" || filename === ""){
		results.innerHTML = "You must fill in the text boxes.";
		return;
	}
	
	for(var i = 0; i < radios.length; i++){
		if(radios[i].checked) {
			mode_selected = true;
			mode = radios[i].value;
		}
	}
	if(!mode_selected){
		results.innerHTML = "Please select a mode";
		return;
	}
	
	if(type === "preview"){
		send_info = {data:input, mode:mode};
	}
	else if (type === "write"){
		send_info = {data:input, mode:mode, file:filename};
	}
	else{
		results.innerHTML = "Internal error: type must be preview or write, but it's "+type;
		return;
	}
	
	$.get("add/"+type,
		send_info,
		function(data){
			results.innerHTML = data;
		});
}