import QtQuick 2.9
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.3
import JASP.Theme 1.0
import JASP.Widgets 1.0

Item
{
	id:rect
	
	Label
	{
		id:headLabel
		
		width:implicitWidth
		height:30
		anchors.top: parent.top
		anchors.left: parent.left  //Position Recent Files label
		anchors.leftMargin: 12
		anchors.topMargin: 12
		text: "Recent Folders"
		verticalAlignment: Text.AlignVCenter
		font.family: "SansSerif"
		font.pixelSize: 18
		color: Theme.black
	}
	
	FilterButton {
		id: browseButton
					
		text: "Browse"
		width: 80
		height: 20
		anchors.left: parent.left
		anchors.top: headLabel.bottom
		anchors.leftMargin: 10
		anchors.topMargin: 10
		
		onClicked: {
			fileMenuModel.computer.browseMostRecent();
		}
	}
	
	ToolSeparator
	{
		id: firstSeparator
		
		anchors.top: browseButton.bottom
		anchors.topMargin: 10
		width: rect.width
		orientation: Qt.Horizontal
	}
	
	FileList {
		id:			computerList
		cppModel:	fileMenuModel.computer.listModel

		anchors
		{
			top:			firstSeparator.bottom
			left:			parent.left
			right:			parent.right
			bottom:			parent.bottom
			leftMargin:		12  //Position datalibrary items
			topMargin:		Theme.generalAnchorMargin
			bottomMargin:	Theme.generalAnchorMargin
			rightMargin:	Theme.generalAnchorMargin
		}
		
	}
	
}
