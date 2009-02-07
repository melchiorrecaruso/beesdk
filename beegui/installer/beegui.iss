; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

[Setup]
AppName=BeeGui
AppVerName=BeeGui 1.0.5 BETA
AppPublisher=Melchiorre Caruso, Correggio (Italy)
AppPublisherURL=http://www.beegui.org
AppSupportURL=http://www.beegui.org
AppUpdatesURL=http://www.beegui.org
DefaultDirName={pf}\BeeGui
DefaultGroupName=BeeGui
AllowNoIcons=yes
LicenseFile=..\license.txt
OutputDir=distribution
OutputBaseFilename=BeeGui105
Compression=lzma
SolidCompression=yes

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked
Name: "quicklaunchicon"; Description: "{cm:CreateQuickLaunchIcon}"; GroupDescription: "{cm:AdditionalIcons}";

[Files]
Source: "..\distribution\BeeGui.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\distribution\*"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\distribution\docs\*"; DestDir: "{app}\docs"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "..\distribution\largeicons\*"; DestDir: "{app}\largeicons"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "..\distribution\smallicons\*"; DestDir: "{app}\smallicons"; Flags: ignoreversion recursesubdirs createallsubdirs

[INI]
Filename: "{app}\BeeGui.url"; Section: "InternetShortcut"; Key: "URL"; String: "http://www.compression.ru/fa"

[Icons]
Name: "{group}\BeeGui"; Filename: "{app}\BeeGui.exe"
Name: "{group}\{cm:ProgramOnTheWeb,BeeGui}"; Filename: "{app}\BeeGui.url"
Name: "{group}\{cm:UninstallProgram,BeeGui}"; Filename: "{uninstallexe}"
Name: "{userdesktop}\BeeGui"; Filename: "{app}\BeeGui.exe"; Tasks: desktopicon
Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\BeeGui"; Filename: "{app}\BeeGui.exe"; Tasks: quicklaunchicon

[Run]
;Filename: "{app}\BeeGui.exe"; Parameters: "register:file"; Description: "Associate BeeGui with .bee files"; Flags: nowait postinstall skipifsilent

[UninstallRun]
;Filename: "{app}\BeeGui.exe"; Parameters: "unregister:file";

[UninstallDelete]
Type: files; Name: "{app}\BeeGui.url"


