''' Plugin for CudaText editor
Authors:
    Andrey Kvichansky    (kvichans on githab.com)
Version:
    '0.9.8 2015-12-15'
'''

from .cd_sess_manager import Command as CommandRLS

RLS  = CommandRLS()
class Command:
	def recent(self):                  return RLS.recent()
	def open(self, ssnew=None):        return RLS.open(ssnew)
	def openPrev(self, recent_pos=1):  return RLS.openPrev(recent_pos)
	def save(self):                    return RLS.save()
	def saveAs(self):                  return RLS.saveAs()
	#class Command

