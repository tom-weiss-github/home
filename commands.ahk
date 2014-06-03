; Comment

F12::
SetTitleMatchMode 2
IfWinExist, emacs
{
  WinActivate
}
return

F11::
SetTitleMatchMode 2
IfWinExist, terminal
{
  WinActivate
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

F10::
IfWinExist, ahk_class Chrome_WidgetWin_1
{
  WinActivate
}
return

F9::
SetTitleMatchMode 2
IfWinExist, Buddy List
{
  WinActivate
}
return

F8::
SetTitleMatchMode 2
IfWinExist, pidgin
{
  WinActivate
}
