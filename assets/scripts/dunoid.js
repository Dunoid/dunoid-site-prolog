var preview_page = function(){
	var radios, mode, input, preview, mode_selected = false;
	
	radios = document.getElementsByName("mode_radio");
	preview = document.getElementById("preview");
	input = document.getElementById("writebox").value;
	
	for(var i = 0; i < radios.length; i++){
		if(radios[i].checked) {
			mode_selected = true;
			mode = radios[i].value;
		}
	}
	if(!mode_selected){
		preview.innerHTML = "Please select a mode";
		return;
	}
	$.get("add/preview",
		{data:input, mode:mode}, 
		function(data){
			alert("Success");
			preview.innerHTML = data;
		});
}