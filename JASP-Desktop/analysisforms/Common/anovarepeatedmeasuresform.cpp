//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

#include "anovarepeatedmeasuresform.h"
#include "ui_anovarepeatedmeasuresform.h"

#include "column.h"
#include "widgets/tablemodelvariablesassigned.h"
#include "widgets/tablemodelanovamodelnuisancefactors.h"

using namespace std;

AnovaRepeatedMeasuresForm::AnovaRepeatedMeasuresForm(QWidget *parent) :
	AnalysisForm("AnovaRepeatedMeasuresForm", parent),
	ui(new Ui::AnovaRepeatedMeasuresForm)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&_availableVariablesModel);

	_designTableModel = new TableModelAnovaDesign(this);
	ui->repeatedMeasuresFactors->setModel(_designTableModel);

	// this is a hack to allow deleting factors and levels :/
	// ideally this would be handled between the TableView and the model
	// and wouldn't require the surrounding classes' intervention like this
	connect(ui->repeatedMeasuresFactors, SIGNAL(clicked(QModelIndex)), this, SLOT(anovaDesignTableClicked(QModelIndex)));

	_withinSubjectCellsListModel = new TableModelAnovaWithinSubjectCells(this);
	_withinSubjectCellsListModel->setSource(&_availableVariablesModel);
	_withinSubjectCellsListModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	_withinSubjectCellsListModel->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->repeatedMeasuresCells->setModel(_withinSubjectCellsListModel);
	ui->repeatedMeasuresCells->viewport()->setAttribute(Qt::WA_Hover);
	ui->repeatedMeasuresCells->setItemDelegate(new CustomHoverDelegate(ui->repeatedMeasuresCells));


	_betweenSubjectsFactorsListModel = new TableModelVariablesAssigned(this);
	_betweenSubjectsFactorsListModel->setSource(&_availableVariablesModel);
	_betweenSubjectsFactorsListModel->setVariableTypesSuggested(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
    _betweenSubjectsFactorsListModel->setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeNominalText | Column::ColumnTypeOrdinal);
    ui->betweenSubjectFactors->setModel(_betweenSubjectsFactorsListModel);

	_covariatesListModel = new TableModelVariablesAssigned(this);
	_covariatesListModel->setSource(&_availableVariablesModel);
	_covariatesListModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	_covariatesListModel->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->covariates->setModel(_covariatesListModel);

	ui->buttonAssignRMCells->setSourceAndTarget(ui->listAvailableFields, ui->repeatedMeasuresCells);
	ui->buttonAssignBSFactors->setSourceAndTarget(ui->listAvailableFields, ui->betweenSubjectFactors);
	ui->buttonAssignCovariates->setSourceAndTarget(ui->listAvailableFields, ui->covariates);

	_withinSubjectsTermsModel = new TableModelAnovaModel(this);
    ui->modelTerms->setModel(_withinSubjectsTermsModel);
	connect(_withinSubjectsTermsModel, SIGNAL(termsChanged()), this, SLOT(termsChanged()));

	_contrastsModel = new TableModelVariablesOptions();
    ui->contrasts->setModel(_contrastsModel);

	connect(_designTableModel, SIGNAL(designChanging()), this, SLOT(factorsChanging()));
	connect(_designTableModel, SIGNAL(designChanged()), this, SLOT(withinSubjectsDesignChanged()));
	connect(_designTableModel, SIGNAL(factorAdded(Terms)), _withinSubjectsTermsModel, SLOT(addFixedFactors(Terms)));
	connect(_designTableModel, SIGNAL(factorRemoved(Terms)), _withinSubjectsTermsModel, SLOT(removeVariables(Terms)));

	connect(_betweenSubjectsFactorsListModel, SIGNAL(assignmentsChanging()), this, SLOT(factorsChanging()));
	connect(_betweenSubjectsFactorsListModel, SIGNAL(assignmentsChanged(bool)), this, SLOT(factorsChanged(bool)));
    connect(_betweenSubjectsFactorsListModel, SIGNAL(assignedTo(Terms)), _withinSubjectsTermsModel, SLOT(addFixedFactors(Terms)));
    connect(_betweenSubjectsFactorsListModel, SIGNAL(unassigned(Terms)), _withinSubjectsTermsModel, SLOT(removeVariables(Terms)));

	connect(_covariatesListModel, SIGNAL(assignmentsChanging()), this, SLOT(factorsChanging()));
	connect(_covariatesListModel, SIGNAL(assignmentsChanged(bool)), this, SLOT(factorsChanged(bool)));
    connect(_covariatesListModel, SIGNAL(assignedTo(Terms)), _withinSubjectsTermsModel, SLOT(addCovariates(Terms)));
    connect(_covariatesListModel, SIGNAL(unassigned(Terms)), _withinSubjectsTermsModel, SLOT(removeVariables(Terms)));

	_plotFactorsAvailableTableModel = new TableModelVariablesAvailable();
	ui->plotVariables->setModel(_plotFactorsAvailableTableModel);

	_horizontalAxisTableModel = new TableModelVariablesAssigned(this);
	_horizontalAxisTableModel->setSource(_plotFactorsAvailableTableModel);
	ui->plotHorizontalAxis->setModel(_horizontalAxisTableModel);

	_seperateLinesTableModel = new TableModelVariablesAssigned(this);
	_seperateLinesTableModel->setSource(_plotFactorsAvailableTableModel);
	ui->plotSeparateLines->setModel(_seperateLinesTableModel);

	_seperatePlotsTableModel = new TableModelVariablesAssigned(this);
	_seperatePlotsTableModel->setSource(_plotFactorsAvailableTableModel);
	ui->plotSeparatePlots->setModel(_seperatePlotsTableModel);

	ui->buttonAssignHorizontalAxis->setSourceAndTarget(ui->plotVariables, ui->plotHorizontalAxis);
	ui->buttonAssignSeperateLines->setSourceAndTarget(ui->plotVariables, ui->plotSeparateLines);
	ui->buttonAssignSeperatePlots->setSourceAndTarget(ui->plotVariables, ui->plotSeparatePlots);

    _simpleEffectsAvailableTableModel = new TableModelVariablesAvailable();
	ui->simpleEffectsVariables->setModel(_simpleEffectsAvailableTableModel);

    _simpleFactorTableModel = new TableModelVariablesAssigned(this);
    _simpleFactorTableModel->setSource(_simpleEffectsAvailableTableModel);
	ui->simpleFactor->setModel(_simpleFactorTableModel);

    _moderatorOneTableModel = new TableModelVariablesAssigned(this);
    _moderatorOneTableModel->setSource(_simpleEffectsAvailableTableModel);
	ui->moderatorFactorOne->setModel(_moderatorOneTableModel);

    _moderatorTwoTableModel = new TableModelVariablesAssigned(this);
    _moderatorTwoTableModel->setSource(_simpleEffectsAvailableTableModel);
	ui->moderatorFactorTwo->setModel(_moderatorTwoTableModel);

	ui->buttonAssignSimpleFactor->setSourceAndTarget(ui->simpleEffectsVariables, ui->simpleFactor);
	ui->buttonAssignModeratorOne->setSourceAndTarget(ui->simpleEffectsVariables, ui->moderatorFactorOne);
	ui->buttonAssignModeratorTwo->setSourceAndTarget(ui->simpleEffectsVariables, ui->moderatorFactorTwo);

		_friedmanAvailableTableModel = new TableModelVariablesAvailable();
	ui->friedmanVariables->setModel(_friedmanAvailableTableModel);

		_friedmanWithinTableModel = new TableModelVariablesAssigned(this);
		_friedmanWithinTableModel->setSource(_friedmanAvailableTableModel);
	ui->friedmanWithinFactor->setModel(_friedmanWithinTableModel);
	
		_friedmanBetweenTableModel = new TableModelVariablesAssigned(this);
		_friedmanBetweenTableModel->setSource(_friedmanAvailableTableModel);
	ui->friedmanBetweenFactor->setModel(_friedmanBetweenTableModel);

	ui->buttonAssignFriedmanWithin->setSourceAndTarget(ui->friedmanVariables, ui->friedmanWithinFactor);
	ui->buttonAssignFriedmanBetween->setSourceAndTarget(ui->friedmanVariables, ui->friedmanBetweenFactor);

	ui->containerModel->hide();
	ui->containerFactors->hide();
	ui->containerOptions->hide();
	ui->containerSimpleEffect->hide();
	ui->containerPostHocTests->hide();
	ui->containerDescriptivesPlot->hide();
	ui->containerAssumptions->hide();
	ui->containerFriedman->hide();


    ui->modelTerms->setFactorsLabel("Components");

	ui->confidenceIntervalInterval->setLabel("Confidence interval");

	connect(_designTableModel, SIGNAL(designChanged()), this, SLOT(withinSubjectsDesignChanged()));

    termsChanged();

}

AnovaRepeatedMeasuresForm::~AnovaRepeatedMeasuresForm()
{
	delete ui;
}

void AnovaRepeatedMeasuresForm::bindTo(Options *options, DataSet *dataSet)
{
	AnalysisForm::bindTo(options, dataSet);

	Terms factors;

	foreach (const Factor &factor, _designTableModel->design())
		factors.add(factor.first);

	_withinSubjectsTermsModel->setVariables(factors);

	if (_withinSubjectsTermsModel->terms().size() == 0)
		_withinSubjectsTermsModel->addFixedFactors(factors);

    _withinSubjectsTermsModel->setVariables(_betweenSubjectsFactorsListModel->assigned());
    termsChanged();
}

void AnovaRepeatedMeasuresForm::factorsChanging()
{
	if (_options != NULL)
		_options->blockSignals(true);
}

void AnovaRepeatedMeasuresForm::factorsChanged(bool changed)
{
	if (changed)
	{
		Terms factorsAvailable;

		foreach (const Factor &factor, _designTableModel->design())
			factorsAvailable.add(factor.first);

		factorsAvailable.add(_betweenSubjectsFactorsListModel->assigned());

		_contrastsModel->setVariables(factorsAvailable);
		_plotFactorsAvailableTableModel->setVariables(factorsAvailable);
		_simpleEffectsAvailableTableModel->setVariables(factorsAvailable);
		_friedmanAvailableTableModel->setVariables(factorsAvailable);

		Terms plotVariablesAssigned;
		plotVariablesAssigned.add(_horizontalAxisTableModel->assigned());
		plotVariablesAssigned.add(_seperateLinesTableModel->assigned());
		plotVariablesAssigned.add(_seperatePlotsTableModel->assigned());
		_plotFactorsAvailableTableModel->notifyAlreadyAssigned(plotVariablesAssigned);

		Terms simpleEffectsVariablesAssigned;
		simpleEffectsVariablesAssigned.add(_simpleFactorTableModel->assigned());
		simpleEffectsVariablesAssigned.add(_moderatorOneTableModel->assigned());
		simpleEffectsVariablesAssigned.add(_moderatorTwoTableModel->assigned());
		_simpleEffectsAvailableTableModel->notifyAlreadyAssigned(simpleEffectsVariablesAssigned);

		Terms friedmanVariablesAssigned;
		friedmanVariablesAssigned.add(_friedmanWithinTableModel->assigned());
		friedmanVariablesAssigned.add(_friedmanBetweenTableModel->assigned());
		_friedmanAvailableTableModel->notifyAlreadyAssigned(friedmanVariablesAssigned);

		ui->postHocTestsVariables->setVariables(factorsAvailable);
	}

	if (_options != NULL)
		_options->blockSignals(false);
}

void AnovaRepeatedMeasuresForm::termsChanged()
{
	Terms terms;

	foreach (const Factor &factor, _designTableModel->design())
		terms.add(factor.first);

	terms.add(_withinSubjectsTermsModel->terms());

	ui->marginalMeansTerms->setVariables(terms);
}

void AnovaRepeatedMeasuresForm::withinSubjectsDesignChanged()
{
	_withinSubjectCellsListModel->setDesign(_designTableModel->design());
	factorsChanged();
}

void AnovaRepeatedMeasuresForm::anovaDesignTableClicked(QModelIndex index)
{
	// the second column contains an X to delete the row

	if (index.column() == 1)
		_designTableModel->removeRow(index.row());
}
