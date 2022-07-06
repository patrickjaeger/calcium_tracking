#@ File    (label = "Input file", style = "file") srcFile
#@ File    (label = "Output directory", style = "directory") outDir
#@ Float   (label = "Particle RADIUS", value= 7.5) particle_radius

# Load python 2 API modules:
import os
import sys
import gc

# Load imageJ API modules:
from ij import IJ, ImagePlus, WindowManager
import ij.measure.ResultsTable as ResultsTable

# Load bioformats modules:
from loci.plugins import BF

# Load trackmate API modules:
from fiji.plugin.trackmate import Model
from fiji.plugin.trackmate import Settings
from fiji.plugin.trackmate import TrackMate
from fiji.plugin.trackmate import SelectionModel
from fiji.plugin.trackmate import Logger
from fiji.plugin.trackmate.detection import LogDetectorFactory
from fiji.plugin.trackmate.tracking import LAPUtils
from fiji.plugin.trackmate.tracking.sparselap import SparseLAPTrackerFactory
from fiji.plugin.trackmate.providers import SpotAnalyzerProvider
from fiji.plugin.trackmate.providers import EdgeAnalyzerProvider
from fiji.plugin.trackmate.providers import TrackAnalyzerProvider
import fiji.plugin.trackmate.visualization.hyperstack.HyperStackDisplayer as HyperStackDisplayer
import fiji.plugin.trackmate.visualization.TrackMateModelView as TrackMateModelView
import fiji.plugin.trackmate.features.FeatureFilter as FeatureFilter
import fiji.plugin.trackmate.extra.spotanalyzer.SpotMultiChannelIntensityAnalyzerFactory as SpotMultiChannelIntensityAnalyzerFactory
import fiji.plugin.trackmate.extra.trackanalyzer.TrackMeanIntensityAnalyzer as TrackMeanIntensityAnalyzer
import fiji.plugin.trackmate.action.ExportStatsToIJAction as ExportStatsToIJAction 
import fiji.plugin.trackmate.action.ExportAllSpotsStatsAction as ExportAllSpotsStatsAction
import fiji.plugin.trackmate.visualization.TrackColorGenerator as TrackColorGenerator
import fiji.plugin.trackmate.visualization.PerTrackFeatureColorGenerator as PerTrackFeatureColorGenerator
import fiji.plugin.trackmate.features.track.TrackIndexAnalyzer as TrackIndexAnalyzer
import fiji.plugin.trackmate.action.CaptureOverlayAction as CaptureOverlayAction

# Get currently selected image or open standard format
		#imp = WindowManager.getCurrentWindow()
#imp = IJ.openImage(str([srcFile]))

# Open bioformats file:
imps = BF.openImagePlus(str(srcFile))
imp = imps.pop(0)

#imp.show()

	# How to get a title from an opened image:
		# These two do the same:
			#imp_title = ImagePlus.getTitle(imp)
			#imp_title = imp.getTitle()
			#print imp_title

#imp_title = imp.getTitle()
#print imp_title
# imp_dir = IJ.getDir(imp_title)  # getDir opens a window for user input

	# How to get the directory:
		#imp_name = os.path.dirname(imp_path)
		#print imp_name

#----------------------------
# Create the model object now
#----------------------------
    
# Initiate model.
model = Model()
    
# Send all messages to ImageJ log window.
#model.setLogger(Logger.IJ_LOGGER)
       
#------------------------
# Prepare settings object
#------------------------
       
settings = Settings()
settings.setFrom(imp)
       
# Configure detector - We use the Strings for the keys
settings.detectorFactory = LogDetectorFactory()
settings.detectorSettings = { 
    'DO_SUBPIXEL_LOCALIZATION' : True,
    'RADIUS' : particle_radius,
    'TARGET_CHANNEL' : 1,
    'THRESHOLD' : 1.0,
    'DO_MEDIAN_FILTERING' : False,
}  
    
# Configure spot filters - Classical filter on quality
#filter1 = FeatureFilter('QUALITY', 30, True)
#settings.addSpotFilter(filter1)
     
# Configure tracker - We want to allow merges and fusions
settings.trackerFactory = SparseLAPTrackerFactory()
settings.trackerSettings = LAPUtils.getDefaultLAPSettingsMap()
settings.trackerSettings['LINKING_MAX_DISTANCE'] = 15.0
settings.trackerSettings['ALLOW_GAP_CLOSING'] = True
settings.trackerSettings['GAP_CLOSING_MAX_DISTANCE'] = 15.0
settings.trackerSettings['MAX_FRAME_GAP'] = 4
settings.trackerSettings['ALLOW_TRACK_SPLITTING'] = False
settings.trackerSettings['ALLOW_TRACK_MERGING'] = False

# Add ALL the feature analyzers known to TrackMate, via providers. 
spotAnalyzerProvider = SpotAnalyzerProvider()
for key in spotAnalyzerProvider.getKeys():
    settings.addSpotAnalyzerFactory( spotAnalyzerProvider.getFactory( key ) )
 
edgeAnalyzerProvider = EdgeAnalyzerProvider()
for  key in edgeAnalyzerProvider.getKeys():
    settings.addEdgeAnalyzer( edgeAnalyzerProvider.getFactory( key ) )
 
trackAnalyzerProvider = TrackAnalyzerProvider()
for key in trackAnalyzerProvider.getKeys():
    settings.addTrackAnalyzer( trackAnalyzerProvider.getFactory( key ) )
    
# Configure track filters - We want to get rid of the two immobile spots at 
# the bottom right of the image. Track displacement must be above 10 pixels.  
#filter2 = FeatureFilter('TRACK_DISPLACEMENT', 10, True)
#settings.addTrackFilter(filter2)

#-------------------
# Instantiate plugin
#-------------------
    
trackmate = TrackMate(model, settings)
       
#--------
# Process
#--------
    
ok = trackmate.checkInput()
if not ok:
    sys.exit(str(trackmate.getErrorMessage()))
    
ok = trackmate.process()
if not ok:
    sys.exit(str(trackmate.getErrorMessage()))
    
#----------------------------
# Display results (graphical)
#----------------------------
     
selectionModel = SelectionModel(model)
#displayer =  HyperStackDisplayer(model, selectionModel, imp)

#displayer.setDisplaySettings(TrackMateModelView.KEY_TRACK_COLORING, PerTrackFeatureColorGenerator(model, TrackIndexAnalyzer.TRACK_INDEX))

#displayer.render()
#displayer.refresh()

# Echo results with the logger we set at start:
#model.getLogger().log(str(model))

#-------------------------
# Export stats to IJ table
#-------------------------

res = ExportAllSpotsStatsAction(selectionModel).execute(trackmate)
#ExportStatsToIJAction(selectionModel).execute(trackmate)

#-------------------------
# Save stats in .csv-file
#-------------------------

# Construct output path.
imp_name = os.path.splitext(os.path.basename(imp.getTitle()))[0]
csv_name = imp_name + ".csv"
outPath = os.path.join(str(outDir), csv_name)
print imp_name

# Save results table.
#IJ.selectWindow("All Spots statistics")
IJ.saveAs(res, "Results", outPath)

# Save video with spots and tracks overlay.
#imp2 = CaptureOverlayAction.capture(trackmate, 1, imp.getImageStackSize()/2)
#outPath2 = os.path.join(str(outDir), "test.tif")
#IJ.saveAs(imp2, "TIFF", outPath2)

# Close windows:
imp.close()
IJ.selectWindow(csv_name)
IJ.run("Close")

#System.gc()
imp.flush()
#imp.trimProcessor()
gc.collect()