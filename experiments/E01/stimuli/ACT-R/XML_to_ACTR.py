import xml.etree.ElementTree as ET
import sys
sys.path.insert(0,'/home/ausmanpa/actr7.x/tutorial/python')
import actr


def parseXMLtoACTR(xmlFile):
    
    # create element tree object
    tree = ET.parse(xmlFile)
  
    # get root element
    root = tree.getroot()
    
    for item in root.iter('mxGeometry'):
        
        xCoord = item.get('x')
        yCoord = item.get('y')
        height = item.get('height')
        width = item.get('width')
        
        actr.add_visicon_features(['SCREEN-X',xCoord,'SCREEN-Y',yCoord,'HEIGHT',height,'WIDTH',width])

