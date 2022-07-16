/*
 * Analysis of calcium signals from images which have
 * 2 channels (blue, green).
 * -- this script only gets the number of nuclei --
 */

// get user input:
#@ File (label = "Input directory", style = "directory") input
#@ File (label = "Output directory", style = "directory") output
#@ String (label = "File suffix", value = ".tf8") suffix

// MAIN SCRIPT ----
run("Bio-Formats Macro Extensions");
processFolder(input);
saveResults(ouput);
close("*");

//// FUNCTIONS ----
function processFolder(input) {
	// scan folders/subfolders/files to find files with matching suffix
	list = getFileList(input);
	list = Array.sort(list);
	for (i = 0; i < list.length; i++) {
		if(File.isDirectory(input + File.separator + list[i]))
			processFolder(input + File.separator + list[i]);
		if(endsWith(list[i], suffix))
			processFile(input, output, list[i]);
	}
}

function processFile(input, output, file) { 
	// segment image and count nulei
	run("Bio-Formats Importer", "open=[" + input + File.separator + file + "] " + 
		"color_mode=Default rois_import=[ROI manager] specify_range view=Hyperstack " + 
		"stack_order=XYCZT c_begin=1 c_end=1 c_step=1 t_begin=1 t_end=1 t_step=1");

	run("Maximum...", "radius=2");
	run("Subtract Background...", "rolling=100");
	run("Bandpass Filter...", "filter_large=30 filter_small=3 suppress=None " +
		"tolerance=5 autoscale saturate");

	setAutoThreshold("Default");
	setOption("BlackBackground", true);
	run("Convert to Mask");
	run("Invert");

	run("Watershed");
	run("Analyze Particles...", "size=50-Infinity show=Nothing exclude summarize");
}

function saveResults(output) {
	// save the results from the particle analyzer
	IJ.renameResults("Summary.csv","Results.csv");
	saveAs("Results", output+File.separator+n_nuclei.csv);
	close("Results.csv");
}