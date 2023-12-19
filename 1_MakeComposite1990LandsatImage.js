
var alladale = ee.FeatureCollection("projects/alladale2023/assets/AlladaleBuffer");

var s88 =ee.Filter.date('1988-05-01','1988-09-30');
var s89 =ee.Filter.date('1989-05-01','1989-09-30');
var s90 =ee.Filter.date('1990-05-01','1990-09-30');
var s91 =ee.Filter.date('1991-05-01','1991-09-30');
var s92 =ee.Filter.date('1992-05-01','1992-09-30');


var allSeasonsFilter = ee.Filter.or(s88,s89,s90,s91,s92); // Create joint Filter

function maskL5srClouds(image) {
  // Bits 3 and 5 are cloud shadow and cloud, respectively.
  var cloudShadowBitMask = (1 << 3);
  var cloudsBitMask = (1 << 5);
  // Get the pixel QA band.
  var qa = image.select('QA_PIXEL');
  // Both flags should be set to zero, indicating clear conditions.
  var mask = qa.bitwiseAnd(cloudShadowBitMask).eq(0)
                .and(qa.bitwiseAnd(cloudsBitMask).eq(0));
  return image.updateMask(mask);
}

var l5 = ee.ImageCollection("LANDSAT/LT05/C02/T1_L2");

var l5Filt = l5.filterBounds(alladale)
               .filter(allSeasonsFilter)
               .filterMetadata('CLOUD_COVER', 'less_than',50)
               .map(maskL5srClouds)
               
               
print('L5 collection filtered and masked',l5Filt);

var composite = l5Filt.median().clip(alladale);

Map.addLayer(composite,{bands:['SR_B3','SR_B2','SR_B1'], min:0, max:15000, gamma:0.25},
             'Composite 1990');
             
print('L5 Composite 1990',composite);

var image = ee.Image('LANDSAT/LC08/C02/T1_L2/LC08_206020_20210827');

print('2021 image',image);

var projection = image.select('SR_B2').projection().getInfo();

print('2021 projection',projection);

// To export to your Google Drive
Export.image.toDrive({
  image: composite,
  description: 'Alladale1990CompositeGDrive', // task name to be shown in the Tasks tab
  fileNamePrefix: 'Alladale1990Composite', // filename to be saved in the Google Drive
  crs: projection.crs,
  crsTransform: projection.transform,
  region: alladale,
  maxPixels: 1e13
});




// get 2007 image so everything matches

var alladale2007 = ee.Image('LANDSAT/LT05/C02/T1_L2/LT05_206020_20070501')
  .select(['SR_B1', 'SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'ST_B6', 'SR_B7']);

print('2007 image',alladale2007);

// To export to your Google Drive
Export.image.toDrive({
  image: alladale2007,
  description: 'Alladale2007GDrive', // task name to be shown in the Tasks tab
  fileNamePrefix: 'Alladale2007', // filename to be saved in the Google Drive
  crs: projection.crs,
  crsTransform: projection.transform,
  region: alladale,
  maxPixels: 1e13
});

