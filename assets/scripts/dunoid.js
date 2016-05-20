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
	
	$.get("add/"+type, send_info, function(data){
		results.innerHTML = data;
	});
};

load_template = function(mode) {
	var art_template = [ 
		"https://link.to.image", 
		"Your description here"
	].join("\n#\n");
		
	var prog_template = [
		"Name of the Project",
		"Technologies Used",
		"Description",
		"Github Link",
		"[Optional] Image Link"
	].join("\n#\n");
	
	var other = "No template for this mode.";
	var write_prompt = "Click a mode to load a template!";
	
	var valid_text = [
		"", //Empty box can be overwritten
		art_template,
		prog_template,
		other,
		write_prompt
	];
	var template;
	
	if(mode === "art"){
		template = art_template;
	}
	else if(mode === "programming"){
		template = prog_template;
	}
	else {
		template = other;
	}
	var wBox = document.getElementById("writebox");
	var previous_text = wBox.value;
	
	if(!isInArray(valid_text, previous_text)){
		
		wBox.value = "Make sure to copy your work before loading a template!\n"+
		previous_text;
	}else{
		wBox.value = template;
	}
};

function isInArray(array, value) {
    return array.indexOf(value) > -1;
};