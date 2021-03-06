import QtQuick 2.9
import QtQuick.Controls 1.4
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.0
import JASP.Theme 1.0

Rectangle
{
	id:				rootDataset
	color:			Theme.uiBackground

    ProgressBarHolder
    {
		id:			progressBarHolder
		objectName: "progressBarHolder"
		visible:	mainWindow.progressBarVisible
    }

    SplitView
    {
        id: splitViewData
		anchors
		{
			top:	progressBarHolder.bottom
			left:	rootDataset.left
			right:	rootDataset.right
			bottom:	rootDataset.bottom
		}

		orientation:	Qt.Vertical
		visible:		!mainWindow.progressBarVisible



        VariablesWindow
        {
            id: variablesWindow
			headersGradient: Gradient{
				GradientStop { position: 0.0;	color: "#EEEEEE" }
				GradientStop { position: 0.75;	color: "#EEEEEE" }
				GradientStop { position: 0.77;	color: "#DDDDDD" }
				GradientStop { position: 1.0;	color: "#DDDDDD" }
			}

			Layout.minimumHeight: calculatedMinimumHeight
        }

        FilterWindow
        {
            id: filterWindow
            objectName: "filterWindow"

			Layout.maximumHeight: rootDataset.height * 0.8
        }

		ComputeColumnWindow
		{
			id: computeColumnWindow
			objectName: "computeColumnWindow"
			Layout.maximumHeight: rootDataset.height * 0.8
		}

		DataTableView
		{
			objectName: "dataSetTableView"
			Layout.fillHeight: true

			signal dataTableDoubleClicked()

			onDoubleClicked: dataTableDoubleClicked()
        }
	}
}
