import xml.etree.ElementTree as ET
import sys
sys.path.insert(0,'/home/ausmanpa/actr7.x/tutorial/python')
#sys.path.insert(0,'/Users/pjr5/actr7.x/tutorial/python')
import actr


def parseXMLtoACTR(xmlFile):
    
    # create element tree object
    tree = ET.parse(xmlFile)
  
    # get root element
    root = tree.getroot()
    
    # for each mxGeometry element in the xml file
    for item in root.iter('mxGeometry'):
        
        # get x/y and height/width of the mxGeometry element
        xCoord = int(item.get('x'))
        yCoord = int(item.get('y'))
        height = int(item.get('height'))
        width = int(item.get('width'))
        
        # add a visicon feature that reflects the x/y and height/width of the mxGeometry element
        vgLabeling.actr.add_visicon_features(['ISA',['VISUAL-LOCATION', 'OVAL'],'SCREEN-X',xCoord,'SCREEN-Y',yCoord,'HEIGHT',height,'WIDTH',width])
        
        
        
def parseXMLtoACTR2(xmlFile):
    
    # create element tree object
    tree = ET.parse(xmlFile)
  
    # get root element
    root = tree.getroot()
    
    # open an act window and install the device
    window = actr.open_exp_window('test', width=850, height=1100, visible=True)
    actr.install_device(window)
    
    # for each mxGeometry element in the xml file
    for item in root.iter('mxGeometry'):
        
        # get x/y and height/width of the mxGeometry element
        xCoord = int(item.get('x'))
        yCoord = int(item.get('y'))
        height = int(item.get('height'))
        width = int(item.get('width'))
        
        # currently, all mxGeometry elements are squares/rectangles
        # compute the x/y start/end points of the four lines that make up the element and add
        # them to the experiment window
        actr.add_line_to_exp_window(window,[xCoord,yCoord],[xCoord+width,yCoord])
        actr.add_line_to_exp_window(window,[xCoord,yCoord],[xCoord,yCoord+height])
        actr.add_line_to_exp_window(window,[xCoord+width,yCoord],[xCoord+width,yCoord+height])
        actr.add_line_to_exp_window(window,[xCoord,yCoord+height],[xCoord+width,yCoord+height])
        

