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

#ifndef ENGINESYNC_H
#define ENGINESYNC_H

#ifdef __APPLE__
#include <semaphore.h>
#else
#include <boost/interprocess/sync/named_semaphore.hpp>
#endif

#include <boost/interprocess/sync/interprocess_mutex.hpp>

#include "enginerepresentation.h"

/* EngineSync is responsible for launching the background
 * processes, scheduling analyses, and for sending and
 * receiving communications with the running analyses.
 * It keeps track of which analyses are executing on
 * which background process.
 */
class EngineSync : public QObject
{
	Q_OBJECT

public:
	EngineSync(Analyses *analyses, DataSetPackage *package, DynamicModules *dynamicModules, QObject *parent);
	~EngineSync();

	void start();
	bool engineStarted()			{ return _engineStarted; }

public slots:
	void sendFilter(QString generatedFilter, QString filter, int requestID);
	void sendRCode(QString rCode, int requestId);
	void computeColumn(QString columnName, QString computeCode, Column::ColumnType columnType);
	void pause();
	void resume();
	
signals:
	void processNewFilterResult(std::vector<bool> filterResult, int requestID);
	void processFilterErrorMsg(QString error, int requestID);
	void engineTerminated();
	void filterUpdated(int requestID);
	void filterErrorTextChanged(QString error);

	void rCodeReturned(QString result, int requestId);

	void ppiChanged(int newPPI);
	void imageBackgroundChanged(QString value);
	void computeColumnSucceeded(std::string columnName, std::string warning, bool dataChanged);
	void computeColumnFailed(std::string columnName, std::string error);

	void moduleInstallationSucceeded(	std::string moduleName);
	void moduleInstallationFailed(		std::string moduleName, std::string errorMessage);
	void moduleLoadingSucceeded(		std::string moduleName);
	void moduleLoadingFailed(			std::string moduleName, std::string errorMessage);

private:
	bool		idleEngineAvailable();
	bool		allEnginesPaused();
	bool		allEnginesResumed();
	QProcess*	startSlaveProcess(int no);
	void		processScriptQueue();
	void		processDynamicModules();
	void		checkModuleWideCastDone();
	void		resetModuleWideCastVars();
	bool		amICastingAModuleRequestWide()	{ return !_requestWideCastModuleJson.isNull(); }

private slots:
	void ProcessAnalysisRequests();
	void deleteOrphanedTempFiles();
	void heartbeatTempFiles();

	void process();

	void subProcessStandardOutput();
	void subProcessStandardError();
	void subProcessStarted();
	void subProcessError(QProcess::ProcessError error);
	void subprocessFinished(int exitCode, QProcess::ExitStatus exitStatus);

	void moduleLoadingFailedHandler(		std::string moduleName, std::string errorMessage, int channelID);
	void moduleLoadingSucceededHandler(		std::string moduleName, int channelID);

private:
	Analyses		*_analyses;
	bool			_engineStarted = false;
	DataSetPackage	*_package;
	DynamicModules	*_dynamicModules = nullptr;;

	std::queue<RScriptStore*>			_waitingScripts;
	std::vector<EngineRepresentation*>	_engines;
	RFilterStore						*_waitingFilter = nullptr;

	std::string _memoryName,
				_engineInfo;

	std::string					_requestWideCastModuleName	= "";
	Json::Value					_requestWideCastModuleJson	= Json::nullValue;
	std::map<int, std::string>	_requestWideCastModuleResults;
};

#endif // ENGINESYNC_H
