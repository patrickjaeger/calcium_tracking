/*
 * open images and combine them to form an OVERVIEW image
 */
close("*");
// specify paths and criteria for file inclusion:
#@ File(label = "Input directory", style = "directory") input
#@ String(label="Pattern", value="_no_") pattern
#@ String(label="Suffix", value=".tf8") suffix

// load packages:
run("Bio-Formats Macro Extensions");

// read file paths:
filelist = getFileList(input);
	//Array.show(filelist);

// filter files:
files = newArray();
for (i = 0; i < lengthOf(filelist); i++) {
    if (matches(filelist[i], ".*"+ pattern +".*") && endsWith(filelist[i], suffix)) { 
    	files = Array.concat(files, filelist[i]);
    } 
}
	//Array.show(files);

// open and reformat files:
for (i = 0; i < lengthOf(files); i++) {
	run("Bio-Formats Importer", 
	"open=" + input + File.separator + files[i] + 
	" color_mode=Default rois_import=[ROI manager] specify_range" +
	" view=Hyperstack stack_order=XYCZT t_step=12");

	run("Split Channels");
	close("C1*");
	run("Enhance Contrast", "saturated=0.35");
	run("8-bit");
	run("Size...", "width=350 height=321 time="+ nSlices +" constrain average interpolation=Bicubic");
}

// combine images:
imgs = getList("image.titles");
run("Combine...", "stack1=[" +imgs[0] +"] stack2=["+ imgs[1]+"]");

if (lengthOf(imgs) > 2) {
	for (i = 0; i < (lengthOf(imgs)-1); i++) {
		imgs = getList("image.titles");
		imgs = Array.reverse(imgs);
		run("Combine...", "stack1=[" +imgs[0] +"] stack2=["+ imgs[1]+"]");
	}
}
new_title = pattern+"_overview";
rename(new_title);

// save image:
saveAs("Tiff", input+File.separator+new_title+".tif");
