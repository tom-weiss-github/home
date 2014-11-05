; To launch automatically when Windows 7 starts:
; 1) Copy (ctrl-c) this file
; 2) Open C:\programdata\microsoft\windows\start menu\programs\startup
; 3) Paste as shortcut

; # is the win key
#j::
SetTitleMatchMode 2
IfWinExist, emacs
{
  WinActivate
}
return

#k::
SetTitleMatchMode 2
; I set the title of the terminal to be "term | (stuff)", but ssh tends
; to rename when I ssh to other machines.
if WinExist("term") or WinExist("root@")
{
  WinActivate ; Uses the last found window.
}
else
{
  IfWinExist, MobaXterm
  {
    WinActivate
  }
  else
  {
    Run "C:\Program Files (x86)\Mobatek\MobaXterm Personal Edition\MobaXterm.exe"
	WinWait MobaXterm
	WinActivate
  }
}
return

; b for browser
#b::
IfWinExist, ahk_class Chrome_WidgetWin_1
{
  WinActivate
}
return

; p for pidgin
#p::
SetTitleMatchMode 2
IfWinExist, Buddy List
{
  WinActivate
}
return

