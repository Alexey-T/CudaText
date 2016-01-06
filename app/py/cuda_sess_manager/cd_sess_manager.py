''' Plugin for CudaText editor
Authors:
    Andrey Kvichansky    (kvichans on githab.com)
Version:
    '0.9.8 2015-12-15'
'''
import cudatext as app
import cudatext_cmd as cmds
import os
import json

FILE_EXT		= '.cuda-session'
# Localization
NEED_NEWER_API	= 'Plugin needs newer app version'
NO_RECENT		= 'No recent sessions'
NO_PREV			= 'No previous session'
SAVED			= 'Session "{stem}" is saved'
OPENED			= 'Session "{stem}" is opened'
CREATE_ASK		= 'Session "{stem}" not found\n\nCreate it?'
CREATED			= 'Session "{stem}" is created'
DLG_FILE_FILTER	= 'CudaText sessions|*{}|All files|*.*'.format(FILE_EXT)

class Command:
	def recent(self):
		''' Show list, use user select '''
		if not _checkAPI():	return
		sess	= self._loadSess(existing=True)
		rcnt	= sess['recent']
		if 0==len(rcnt):
			return app.msg_status(NO_RECENT)
		if sess['opts'].get('full_path_in_recent', False):
			ssmenu	= '\n'.join(rcnt)
		else:
#			ssmenu	= '\n'.join(	('{} ({})'.format(juststem(sfile), os.path.dirname(sfile))
			ssmenu	= '\n'.join(	('{}\t{}'.format(juststem(sfile), os.path.dirname(sfile))
										for sfile in rcnt
									) )
#			ssmenu	= ''
#			for sfile in rcnt:
#				ssmenu	= '{pr}\n{stem} ({dir})'.format(ssmenu, juststem(sfile), os.path.dirname(sfile))
		ans		= app.dlg_menu(app.MENU_LIST, ssmenu)
		if ans is None:	return
		self.open(rcnt[ans])

	def open(self, ssnew=None):
		''' Open new session from file ssnew or after user asking '''
		if not _checkAPI():	return
#		in_dir		= app.app_path(app.APP_DIR_DATA)
		sscur		= app.app_path(app.APP_FILE_SESSION)
		if ssnew is None:
			ssnew	= app.dlg_file(is_open=True, filters=DLG_FILE_FILTER
					, init_filename='!'		# '!' to disable check "filename exists"
					, init_dir=		''
					)
		if ssnew is None: return
		ssnew 		= icase(False,''
					,	ssnew.endswith(FILE_EXT), ssnew
					,	os.path.isfile(ssnew)	, ssnew
					,	True					, ssnew+FILE_EXT
					)
		if os.path.isfile(ssnew):
			# Open
			app.app_proc(app.PROC_SAVE_SESSION, sscur)
			app.app_proc(app.PROC_LOAD_SESSION, ssnew)
			app.app_proc(app.PROC_SET_SESSION,  ssnew)
			app.msg_status(OPENED.format(stem=juststem(ssnew)))
			self.top_sess(ssnew)
		else:
			# New
			if app.ID_NO==app.msg_box(CREATE_ASK.format(stem=juststem(ssnew)), app.MB_YESNO):	return
			app.app_proc(app.PROC_SAVE_SESSION, sscur)
			app.ed.cmd(cmds.cmd_FileCloseAll)
			app.app_proc(app.PROC_SET_SESSION,  ssnew)
			app.app_proc(app.PROC_SAVE_SESSION, ssnew)
			app.msg_status(CREATED.format(stem=juststem(ssnew)))
			self.top_sess(ssnew)

	def openPrev(self, recent_pos=1):
		''' Open session that was opened before.
			Params
				recent_pos	Position in recent list
		'''
		if not _checkAPI():	return
		sess	= self._loadSess(existing=True)
		rcnt	= sess['recent']
		if len(rcnt)<1+recent_pos:
			return app.msg_status(NO_PREV)
		self.open(rcnt[recent_pos])

	def save(self):
		''' Save cur session to file '''
		if not _checkAPI():	return
		sscur		= app.app_path(app.APP_FILE_SESSION)
		app.app_proc(app.PROC_SAVE_SESSION, sscur)
		app.msg_status(SAVED.format(stem=juststem(sscur)))
		self.top_sess(sscur)

	def saveAs(self):
		''' Save cur session to new file '''
		if not _checkAPI():	return
		sscur		= app.app_path(app.APP_FILE_SESSION)
		pass;					app.msg_status(sscur)
		(ssdir
		,ssfname)	= os.path.split(sscur)
		ssfname		= ssfname.replace('.json', '')
		ssnew		= app.dlg_file(is_open=False, filters=DLG_FILE_FILTER
					, init_filename=ssfname
					, init_dir=		ssdir
					)
		pass;					app.msg_status(str(ssnew))
		if ssnew is None:	return
		ssnew 		= icase(False,''
					,	ssnew.endswith(FILE_EXT), ssnew
					,	os.path.isfile(ssnew)	, ssnew
					,	True					, ssnew+FILE_EXT
					)
		if os.path.normpath(sscur)==os.path.normpath(ssnew): return
		app.app_proc(app.PROC_SAVE_SESSION, sscur)
		app.app_proc(app.PROC_SAVE_SESSION, ssnew)
		app.app_proc(app.PROC_SET_SESSION,  ssnew)
		app.msg_status(SAVED.format(stem=juststem(ssnew)))
		self.top_sess(ssnew)

	#################################################
	## Private
	def top_sess(self, ssPath):
		''' Set the session on the top of recent.
			Params:
				ssPath	Full path to session file
		'''
		ssPath	= os.path.normpath(ssPath)
		sess	= self._loadSess()
		rcnt	= sess['recent']
		if ssPath in rcnt:
			pos	= rcnt.index(ssPath)
			if 0==pos:	return	# Already at top
			del rcnt[pos]
		rcnt.insert(0, ssPath)
		max_len	= sess['opts']['max_items']
		del rcnt[max_len:]
		self._saveSess(sess)

	def _loadSess(self, existing=False):
		''' See _saveSess for returned data format.
			Params
				existing	Delete path from recent if one doesnot exist
		'''
		sess_json	= os.path.join(app.app_path(app.APP_DIR_SETTINGS), '{}.json'.format(__name__))
		sess	= json.loads(open(sess_json).read())	if os.path.exists(sess_json) else self.dfltSess
		rcnt	= sess['recent']
		if existing and 0<len(rcnt):
			sess['recent']	= list(filter(os.path.isfile, rcnt))
		return sess

	def _saveSess(self, sess):
		''' sess py-format:
				{	'recent':[f1, f2, ...]		# Session fullpaths
				,	'opts':{
					,	'max_items':15			# Max saved list
				}
		'''
		sess_json	= os.path.join(app.app_path(app.APP_DIR_SETTINGS), '{}.json'.format(__name__))
		open(sess_json, 'w').write(json.dumps(sess, indent=2))

	def __init__(self):
		self.dfltSess	=	{	'recent':[]
							,	'opts':{
									'max_items':15
								,	'full_path_in_recent':False
								}
							}

def _checkAPI():
	if app.app_api_version()<'1.0.106':
		app.msg_status(NEED_NEWER_API)
		return False
	return True

#### Utils ####
def juststem(sspath):
	stem_ext	= os.path.basename(sspath)
	return stem_ext[:stem_ext.rindex('.')]

def icase(*pars):
	""" Params	cond1,val1[, cond2,val2, ...[, valElse]...]
		Result	Value for first true cond in pairs otherwise last odd param or None
		Examples
			icase(1==2,'a', 3==3,'b') == 'b'
			icase(1==2,'a', 3==4,'b', 'c') == 'c'
			icase(1==2,'a', 3==4,'b') == None
	"""
	for ppos in range(1,len(pars),2) :
		if pars[ppos-1] :
			return pars[ppos]
	return pars[-1] if 1==len(pars)%2 else None
	#def icase
