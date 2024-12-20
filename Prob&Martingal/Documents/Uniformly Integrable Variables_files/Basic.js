//Basic page scripts
"use strict";

const units = document.getElementsByClassName("unit");
const details = document.getElementsByTagName("details");
const refs = document.getElementsByClassName("ref");

//Open ancillary page
function openAncillary(url){
	const options = "toolbar=0, location=0, directories=0, status=1, menubar=0, scrollbars=1, resizable=0, top=20, left=20, width=600, height=600";
	const win = window.open(url,"_blank", options);
	win.focus();
}
//Expand details
function expandDetails(b){
	let l = details.length;
	for (let i = 0; i < l; i++) details[i].open = b;
}
//Get references
function getReferences() {
	let a = 0, b = 0, ref = "", href = "";
	for (let i = 0; i < refs.length; i++) {
		href = refs[i].href;
		a = href.indexOf("#");
		b = href.length;
		ref = href.substring(a + 1, b);
		for (let j = 0; j < units.length; j++) {
			if (units[j].id == ref) refs[i].innerHTML = "[" + (j + 1) + "]";
		}
	}
}


