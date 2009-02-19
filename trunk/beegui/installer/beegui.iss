; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

[Setup]
AppName=BeeGui
AppVerName=BeeGui 1.0.5 Beta
AppPublisher=BeeGui Team
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
ChangesAssociations=yes

VersionInfoVersion=1.0.5.640
VersionInfoDescription=BeeGui archiver installer
VersionInfoCopyright=GNU General Public License
VersionInfoCompany=BeeGui Team
VersionInfoProductName=BeeGui, the GUI for Bee archiver utility
VersionInfoProductVersion=1.0.5.640

[Tasks]
Name: "desktopicon";     Description: "{cm:CreateDesktopIcon}";     GroupDescription: "{cm:AdditionalIcons}";      Flags: unchecked
Name: "quicklaunchicon"; Description: "{cm:CreateQuickLaunchIcon}"; GroupDescription: "{cm:AdditionalIcons}";
Name: "associatebee";    Description: """.bee"" files";             GroupDescription: "Associate file extension:";

[Files]
Source: "..\distribution\BeeGui.exe";   DestDir: "{app}";            Flags: ignoreversion;
Source: "..\distribution\*";            DestDir: "{app}";            Flags: ignoreversion
Source: "..\distribution\docs\*";       DestDir: "{app}\docs";       Flags: ignoreversion recursesubdirs createallsubdirs
Source: "..\distribution\languages\*";  DestDir: "{app}\languages";  Flags: ignoreversion recursesubdirs createallsubdirs
Source: "..\distribution\largeicons\*"; DestDir: "{app}\largeicons"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "..\distribution\smallicons\*"; DestDir: "{app}\smallicons"; Flags: ignoreversion recursesubdirs createallsubdirs
;Source: "..\distribution\plug-ins\*";   DestDir: "{app}\plug-ins";   Flags: ignoreversion recursesubdirs createallsubdirs

[INI]
Filename: "{app}\BeeGui.url"; Section: "InternetShortcut"; Key: "URL"; String: "http://www.beegui.org"

[Icons]
Name: "{group}\BeeGui";                                                Filename: "{app}\BeeGui.exe"
Name: "{group}\{cm:ProgramOnTheWeb,BeeGui}";                           Filename: "{app}\BeeGui.url"
Name: "{group}\{cm:UninstallProgram,BeeGui}";                          Filename: "{uninstallexe}"
Name: "{userdesktop}\BeeGui";                                          Filename: "{app}\BeeGui.exe"; Tasks: desktopicon
Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\BeeGui"; Filename: "{app}\BeeGui.exe"; Tasks: quicklaunchicon

[Registry]
Root: HKCR; Subkey: ".bee";                      ValueType: string; ValueName: ""; ValueData: "BeeGui";       Flags: uninsdeletevalue; Tasks: associatebee
Root: HKCR; Subkey: "BeeGui";                    ValueType: string; ValueName: ""; ValueData: "Bee Archiver"; Flags: uninsdeletekey;   Tasks: associatebee
Root: HKCR; Subkey: "BeeGui\DefaultIcon";        ValueType: string; ValueName: ""; ValueData: "{app}\BeeGui.exe,0";                    Tasks: associatebee
Root: HKCR; Subkey: "BeeGui\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\BeeGui.exe"" ""O"" ""%1""";     Tasks: associatebee

[UninstallDelete]
Type: files; Name: "{app}\BeeGui.url"

[Code]
function NeedRestart(): Boolean;
var
  ValueData: string;
begin
  if RegQueryStringValue(HKEY_LOCAL_MACHINE, 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment', 'PATH', ValueData) then
  begin
    if (Length(ValueData) > 0) and (ValueData[Length(ValueData)] <> ';') then
    begin
      ValueData := ValueData + ';'
    end;
  end else
    ValueData := '';
  
  if Pos(ExpandConstant('{app}') + ';', ValueData) = 0 then
  begin
    ValueData := ValueData + ExpandConstant('{app}');
    Result := RegWriteStringValue(HKEY_LOCAL_MACHINE, 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment', 'PATH', ValueData);
  end else
    Result := False;
end;

function UninstallNeedRestart(): Boolean;
var
  ValueData: string;
begin
  if RegQueryStringValue(HKEY_LOCAL_MACHINE, 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment', 'PATH', ValueData) then
  begin
    if (Length(ValueData) > 0) and (ValueData[Length(ValueData)] <> ';') then
    begin
      ValueData := ValueData + ';'
    end;
  end else
    ValueData := '';
    
  if Pos(ExpandConstant('{app}') + ';', ValueData) > 0 then
  begin
    Delete(ValueData, Pos(ExpandConstant('{app}') + ';', ValueData), Length(ExpandConstant('{app}') + ';'));
    Result := RegWriteStringValue(HKEY_LOCAL_MACHINE, 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment', 'PATH', ValueData);
  end else
    Result := False;
end;

