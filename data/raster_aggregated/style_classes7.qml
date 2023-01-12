<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>
<qgis minScale="1e+08" version="3.22.0-Białowieża" styleCategories="AllStyleCategories" hasScaleBasedVisibilityFlag="0" maxScale="0">
  <flags>
    <Identifiable>1</Identifiable>
    <Removable>1</Removable>
    <Searchable>1</Searchable>
    <Private>0</Private>
  </flags>
  <temporal enabled="0" mode="0" fetchMode="0">
    <fixedRange>
      <start></start>
      <end></end>
    </fixedRange>
  </temporal>
  <customproperties>
    <Option type="Map">
      <Option name="WMSBackgroundLayer" value="false" type="bool"/>
      <Option name="WMSPublishDataSourceUrl" value="false" type="bool"/>
      <Option name="embeddedWidgets/count" value="0" type="int"/>
      <Option name="identify/format" value="Value" type="QString"/>
    </Option>
  </customproperties>
  <pipe-data-defined-properties>
    <Option type="Map">
      <Option name="name" value="" type="QString"/>
      <Option name="properties"/>
      <Option name="type" value="collection" type="QString"/>
    </Option>
  </pipe-data-defined-properties>
  <pipe>
    <provider>
      <resampling enabled="false" zoomedInResamplingMethod="nearestNeighbour" zoomedOutResamplingMethod="nearestNeighbour" maxOversampling="2"/>
    </provider>
    <rasterrenderer alphaBand="-1" nodataColor="" opacity="1" type="paletted" band="1">
      <rasterTransparency/>
      <minMaxOrigin>
        <limits>None</limits>
        <extent>WholeRaster</extent>
        <statAccuracy>Estimated</statAccuracy>
        <cumulativeCutLower>0.02</cumulativeCutLower>
        <cumulativeCutUpper>0.98</cumulativeCutUpper>
        <stdDevFactor>2</stdDevFactor>
      </minMaxOrigin>
      <colorPalette>
        <paletteEntry color="#5a5a5a" alpha="255" label="1-urbanized" value="1"/>
        <paletteEntry color="#00afff" alpha="255" label="2-water" value="2"/>
        <paletteEntry color="#ffff7d" alpha="255" label="3-agriculture" value="3"/>
        <paletteEntry color="#dfa839" alpha="255" label="4-beach" value="4"/>
        <paletteEntry color="#3bab03" alpha="255" label="5-scrub" value="5"/>
        <paletteEntry color="#4c7300" alpha="255" label="6-fixed" value="6"/>
        <paletteEntry color="#ffffbe" alpha="255" label="7-open dune" value="7"/>
      </colorPalette>
      <colorramp name="[source]" type="randomcolors">
        <Option/>
      </colorramp>
    </rasterrenderer>
    <brightnesscontrast gamma="1" brightness="0" contrast="0"/>
    <huesaturation colorizeRed="255" grayscaleMode="0" colorizeBlue="128" saturation="0" colorizeGreen="128" colorizeOn="0" invertColors="0" colorizeStrength="100"/>
    <rasterresampler maxOversampling="2"/>
    <resamplingStage>resamplingFilter</resamplingStage>
  </pipe>
  <blendMode>0</blendMode>
</qgis>
