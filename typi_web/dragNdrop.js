/*
filedrag.js - HTML5 File Drag & Drop demonstration
Featured on SitePoint.com
Developed by Craig Buckler (@craigbuckler) of OptimalWorks.net
*/
(function() {

    // getElementById
    function $id(id) {
	return document.getElementById(id);
    }
    
    // output information
    function Output(msg) {
	var m = $id("console");
	m.value = m.value+msg+"\n";
    }
    
    // file drag hover
    function FileDragHover(e) {
	e.stopPropagation();
	e.preventDefault();
	e.target.className = (e.type == "dragover" ? "hover" : "");
    }

    // file selection
    function FileSelectHandler(e) {	
	// cancel event and hover styling
	FileDragHover(e);
	
	// fetch FileList object
	var files = e.target.files || e.dataTransfer.files;
	
	// process all File objects
	for (var i = 0, f; f = files[i]; i++) {
	    ParseFile(f);
	}
    }
    
    // output file information
    function ParseFile(file) {	
	// display an image
	if (file.type.indexOf("image") == 0) {
	   alert("What the **** do you want me to do with this?");
	}
	
	// display text
	if (file.type.indexOf("text") == 0) {
	    var reader = new FileReader();
	    reader.onload = function(e) {
		Output(e.target.result);
	    }
	    reader.readAsText(file);
	}
	
    }
    
    
    // initialize
    function Init() {
	
	//var fileselect = $id("fileselect");
	var filedrag = $id("console");
	
	// file select
	//fileselect.addEventListener("change", FileSelectHandler, false);
	
	// is XHR2 available?
	var xhr = new XMLHttpRequest();
	if (xhr.upload) {
	    
	    // file drop
	    filedrag.addEventListener("dragover", FileDragHover, false);
	    filedrag.addEventListener("dragleave", FileDragHover, false);
	    filedrag.addEventListener("drop", FileSelectHandler, false);
	}
	
    }
    
    // call initialization file
    if (window.File && window.FileList && window.FileReader) {
	Init();
    }
    
    
})();
