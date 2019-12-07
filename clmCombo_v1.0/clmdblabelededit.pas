unit clmDBLabeledEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, DB, DBCtrls, Lmessages, LCLType;

type
  TclmDatabaseFormat = (clmDefault, clmSqlite);

  TclmDBLabeledEdit = class(TCustomLabeledEdit)
  private
    FDataLink: TFieldDataLink;
    FDatabaseFormat : TclmDatabaseFormat;

    procedure DataChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);
    procedure FocusRequest(Sender: TObject);

    function GetDatabaseFormat : TclmDatabaseFormat;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;

    function IsReadOnly: boolean;

    Procedure SetDatabaseFormat(Value  : TclmDatabaseFormat);
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure CMGetDataLink(var Message: TLMessage); message CM_GETDATALINK;

  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ActiveChange(Sender: TObject); virtual;
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure KeyPress(var Key: char); override;
    procedure DoEnter; override;
    function GetReadOnly: boolean; override;
    procedure SetReadOnly(Value: boolean); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure EditingDone; override;
    property Field: TField read GetField;
  published
    property DataFormat: TclmDatabaseFormat read GetDatabaseFormat write SetDatabaseFormat;

    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: boolean read GetReadOnly write SetReadOnly default False;

    property Action;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DragCursor;
    property DragMode;
    property EditLabel;
    property Enabled;
    property Font;
    property LabelPosition;
    property LabelSpacing;
    property MaxLength;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnUTF8KeyPress;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('clm',[TclmDBLabeledEdit]);
end;

function FieldIsEditable(Field: TField): boolean;
begin
  Result := (Field <> nil) and (not Field.Calculated) and
    (Field.DataType <> ftAutoInc) and (not Field.Lookup);
end;


{ TJDBLabeledEdit }

procedure TclmDBLabeledEdit.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
    Caption := FDataLink.Field.AsString
  else
    Text := '';
end;

procedure TclmDBLabeledEdit.UpdateData(Sender: TObject);
begin
  if FDataLink.Field <> nil then
    Begin
      If FDatabaseFormat.clmDefault = clmDefault Then
        Begin
          FDataLink.Field.Text := Text;
        end
      Else
        Begin // Sqllite

        end;
    end
  Else
    Text := '';
end;

procedure TclmDBLabeledEdit.FocusRequest(Sender: TObject);
begin
  SetFocus;
end;

function TclmDBLabeledEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TclmDBLabeledEdit.GetDatabaseFormat: TclmDatabaseFormat;
begin
  Result := FDatabaseFormat;
end;

procedure TclmDBLabeledEdit.SetDatabaseFormat(Value : TclmDatabaseFormat);
begin
  FDatabaseFormat := Value;
end;

function TclmDBLabeledEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TclmDBLabeledEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TclmDBLabeledEdit.IsReadOnly: boolean;
begin
  if FDatalink.Active then
    Result := not FDatalink.CanModify
  else
    Result := False;
end;

procedure TclmDBLabeledEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
  if FDataLink.Field <> nil then
    MaxLength := FDataLink.Field.Size
  else
    MaxLength := 0;
end;

procedure TclmDBLabeledEdit.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    ChangeDataSource(Self, FDataLink, Value);
end;

procedure TclmDBLabeledEdit.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink);
end;

procedure TclmDBLabeledEdit.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then
    DataChange(Self);
end;

procedure TclmDBLabeledEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  // clean up
  if (Operation = opRemove) then
  begin
    if (FDataLink <> nil) and (AComponent = DataSource) then
      DataSource := nil;
  end;
end;

procedure TclmDBLabeledEdit.ActiveChange(Sender: TObject);
begin
  if FDatalink.Active then
    datachange(Sender)
  else
    Text := '';
end;

procedure TclmDBLabeledEdit.KeyDown(var Key: word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Key = VK_ESCAPE then
  begin
    FDataLink.Reset;
    SelectAll;
    Key := VK_UNKNOWN;
  end
  else
  if Key in [VK_DELETE, VK_BACK] then
  begin
    if not IsReadOnly then
      FDatalink.Edit
    else
      Key := VK_UNKNOWN;
  end;
end;

procedure TclmDBLabeledEdit.KeyPress(var Key: char);
begin
  if not FieldIsEditable(Field) or not FDatalink.Edit then
    Key := #0;
  inherited KeyPress(Key);
end;

procedure TclmDBLabeledEdit.DoEnter;
begin
  if not FieldIsEditable(Field) or IsReadOnly then
    exit;
  if FDataLink.Field <> nil then
    Caption := FDataLink.Field.AsString;
  inherited DoEnter;
end;

function TclmDBLabeledEdit.GetReadOnly: boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TclmDBLabeledEdit.SetReadOnly(Value: boolean);
begin
  inherited;
  FDataLink.ReadOnly := Value;
end;

constructor TclmDBLabeledEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := @DataChange;
  FDataLink.OnUpdateData := @UpdateData;
  FDataLInk.OnActiveChange := @ActiveChange;
end;

destructor TclmDBLabeledEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TclmDBLabeledEdit.EditingDone;
begin
  inherited EditingDone;
  if not FieldIsEditable(Field) or IsReadOnly then
    exit;
  if DataSource.State in [dsEdit, dsInsert] then
    UpdateData(self);
end;


end.
