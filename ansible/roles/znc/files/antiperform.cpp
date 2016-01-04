/*
 * Copyright (C) 2004-2013  See the AUTHORS file for details.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 as published
 * by the Free Software Foundation.
 *
 * antiperform.cpp - Copied from werring's gist found here:
 * https://gist.github.com/werring/472170
 * I'm assuming this was based on work from Un1matr1x, but I can't say for
 * sure. If one of you wants to be credited properly let me know.
 *
 * Resurrected by zomg in 2013 because people whined and I was feeling nice.
 * - For Carl -- He who wanted to protect the SwiftIRCs but didn't want to log
 * the connect notices when he disconnected. Good thinking, Batman. Carl4NA.
 *
 * Confirmed to work on ZNC 1.0 but if you find something strange or even want
 * to make this better please have at it!
 */

#include <znc/main.h>
#include <znc/Modules.h>
#include <znc/User.h>
#include <algorithm>
 
class CAntiPerform : public CModule {
public:
	MODCONSTRUCTOR(CAntiPerform) {} 
	virtual ~CAntiPerform() {}
 
	virtual bool OnLoad(const CString& sArgs, CString& sMessage) {
		GetNV("Perform").Split("\n", m_vPerform, false);
 
		return true;
	}
 
	virtual void OnModCommand(const CString& sCommand) {
		CString sCmdName = sCommand.Token(0).AsLower();
		if (sCmdName == "add") {
			CString sPerf = sCommand.Token(1, true);
 
			if (sPerf.empty()) {
				PutModule("Usage: add <command>");
				return;
			}
 
			if (sPerf.Left(1) == "/")
				sPerf.LeftChomp();
 
			if (sPerf.Token(0).Equals("MSG")) {
				sPerf = "PRIVMSG " + sPerf.Token(1, true);
			}
 
			if ((sPerf.Token(0).Equals("PRIVMSG") ||
				sPerf.Token(0).Equals("NOTICE")) &&
				sPerf.Token(2).Left(1) != ":") {
				sPerf = sPerf.Token(0) + " " + sPerf.Token(1)
					+ " :" + sPerf.Token(2, true);
			}
			m_vPerform.push_back(sPerf);
			PutModule("Added!");
			Save();
		} else if (sCmdName == "del") {
			u_int iNum = sCommand.Token(1, true).ToUInt();
			if (iNum > m_vPerform.size() || iNum <= 0) {
				PutModule("Illegal # Requested");
				return;
			} else {
				m_vPerform.erase(m_vPerform.begin() + iNum - 1);
				PutModule("Command Erased.");
			}
			Save();
		} else if (sCmdName == "list") {
			int i = 1;
			CString sExpanded;
			for (VCString::iterator it = m_vPerform.begin(); it != m_vPerform.end(); it++, i++) {
				sExpanded = GetUser()->ExpandString(*it);
				if (sExpanded != *it)
					PutModule(CString(i) + ": " + *it + " (" + sExpanded + ")");
				else
					PutModule(CString(i) + ": " + *it);
			}
			PutModule(" -- End of List");
		} else if (sCmdName == "execute") {
			OnIRCConnected();
			PutModule("perform commands sent");
		} else if (sCmdName == "swap") {
			u_int iNumA = sCommand.Token(1).ToUInt();
			u_int iNumB = sCommand.Token(2).ToUInt();
 
			if (iNumA > m_vPerform.size() || iNumA <= 0 || iNumB > m_vPerform.size() || iNumB <= 0) {
				PutModule("Illegal # Requested");
			} else {
				std::iter_swap(m_vPerform.begin() + (iNumA - 1), m_vPerform.begin() + (iNumB - 1));
				PutModule("Commands Swapped.");
				Save();
			}
		} else {
			PutModule("Commands: add <command>, del <nr>, list, execute, swap <nr> <nr>");
		}
	}
 
	virtual void OnClientDisconnect() {
		if (!m_pUser->GetAllClients().empty())
			return;
		for (VCString::iterator it = m_vPerform.begin();
			it != m_vPerform.end();  it++) {
			PutIRC(GetUser()->ExpandString(*it));
		}
	}
 
private:
	bool Save() {
		CString sBuffer = "";
 
		for (VCString::iterator it = m_vPerform.begin(); it != m_vPerform.end(); it++) {
			sBuffer += *it + "\n";
		}
		SetNV("Perform", sBuffer);
 
		return true;
	}
 
	VCString	m_vPerform;
};
 
MODULEDEFS(CAntiPerform, "perform on client disconnect")

