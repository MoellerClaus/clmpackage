{/***************************************************************************
                            clmCombobox.pas
                              ---------
                          A column combobox
                       the combobox for all purposes
                    Initial Revision : Fri jan 3 2014

***************************************************************************/

*****************************************************************************
*                                                                           *
*                                                                           *
*  See the file COPYING.modifiedLGPL.txt, included in a distribution,       *
*  for details about the copyright.                                         *
*                                                                           *
*  This program is distributed in the hope that it will be useful,          *
*  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
*                                                                           *
*****************************************************************************
}
{

TclmCombobox for Lazarus
No Copyright (C) 2014  Claus Moeller
email: vig.hjaelp@gmail.com

Demo of main features attached. clmCombo function in 2 ways.
1. Easy approach using items with deliters to seperate columns
2. Advanced using the columns. With Columns.Count > 0 advanced is selected.

I you like this component - visit Denmark and spent a lot of money and enjoy.

Special thanks to H Page-Clark 2013 for Colcombo
and to Jesus Reyes Aguilar for his work on StringGrid where I took a lot from.
}


unit clmCombobox;

{$mode objfpc}{$H+}

interface

uses  Classes,
      Controls,
      fgl,
      Forms,
      Graphics,
      LCLProc,
      LCLType,
      LResources,
      StdCtrls,
      SysUtils,
      types,
      Grids;


const
  ColSeparatorMargin = 8;  // adjust to suit

type
  TIntList = specialize TFPGList<integer>;

  TclmCombobox = class;

  { TComboColumn }
  TComboColumn = class(TCollectionItem)
  private
    FWidth     : Integer;
    FAlignment : TAlignment;
    FFont      : TFont;
    FColor     : TColor;
    FVisible   : Boolean;
    procedure SetWidth(const Value: Integer);
    procedure SetAlignment(const Value:tAlignment);
    procedure SetFont(const Value:TFont);
    procedure SetColor(const Value:TColor);
    procedure SetVisible(const Value : Boolean);
  protected

  public
    constructor Create(aCollection:TCollection); override;
    destructor  Destroy; override;
    procedure   Assign(Source: TPersistent); override;
  published
    property Alignment : TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Color     : TColor read FColor write SetColor;
    property Font      : TFont read FFont write SetFont;
    property Visible   : Boolean read FVisible write SetVisible default True;
    property Width     : Integer read FWidth write SetWidth default 100;
  end;

  { TComboColumns }
  TComboColumns = class(TCollection)
  private
    FOwner :  TclmComboBox;
    function  GetItem(Index: Integer): TComboColumn;
    procedure SetItem(Index: Integer; const Value: TComboColumn);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    function  Add:TComboColumn;
    function  Insert(index: Integer): TComboColumn;
    property  Items[Index: Integer]: TComboColumn read GetItem write SetItem;
    constructor Create(AOwner:TclmComboBox);
    function  GetOwner: TPersistent; override;
  end;

  TclmCombobox = class(TCustomComboBox)
  private
    FColumns       : TComboColumns;
    FGrid          : TVirtualGrid;

    FEditColumn    : Integer;
    FCharWidth     : Integer;
    FColumnSpacing : Integer;
    FColumnWidths  : TIntList;
    FDelimiter     : AnsiChar;
    FOffsets       : TIntList;
    FParser        : TStringList;
    FShowColSeparators      : Boolean;
    function GetCharWidth   : Integer;
    function GetColumnCount : Integer;
    procedure DrawItemEvent(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure TruncateText(Sender: TObject);
    procedure SetColumns;
    procedure SetColumnSpacing(AValue: integer);
    procedure SetDelimiter(aDelim: AnsiChar);
    procedure SetShowColSeparators(AValue: Boolean);

    procedure SetEditColumn(Value : Integer);
    function  GetEditText: string;
  protected
    procedure FontChanged(Sender: TObject); override;
    class function GetControlClassDefaultSize: TSize; override;
    procedure GetItems; override;
    procedure SetItems(const Value: TStrings); override;
    procedure SetStyle(Val: TComboBoxStyle); override;
    function  GetCells(ACol, ARow: Integer): string;
    procedure SetCells(ACol, ARow: Integer; const AValue: string);
//    Procedure NoNils;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Clear; override;
    property    ColumnCount: integer read GetColumnCount;
    property    Cells[ACol, ARow: Integer]: string read GetCells write SetCells;
    property    EditText : string read GetEditText;
    function    IndexInColumnOf(aCol : Integer; s : string): Integer;
    function    IndexInRowOf(aRow : Integer; s : string): Integer;
    procedure   AddRow;
  published
    // new properties
    property Columns    : TComboColumns read FColumns write FColumns;
    property EditColumn : Integer read FEditColumn write SetEditColumn default 0;
    property ColumnSpacing: integer read FColumnSpacing write SetColumnSpacing default 2;
    property Delimiter: AnsiChar read FDelimiter write SetDelimiter default #9;
    property ShowColSeparators: Boolean read FShowColSeparators write SetShowColSeparators default False;
    // inherited comboBox properties
    property Align;
    property Anchors;
    property ArrowKeysTraverseList;
    property AutoComplete;
    property AutoCompleteText;
    property AutoDropDown;
    property AutoSelect;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ItemHeight;
    property ItemIndex;
    property Items;
    property ItemWidth;
    property MaxLength;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
//    property OnDrawItem;
    property OnEndDrag;
    property OnDropDown;
    property OnEditingDone;
    property OnEnter;
//    property OnExit;
    property OnGetItems;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnSelect;
    property OnUTF8KeyPress;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
  end;

procedure Register;

implementation

Const DummyChar : Char = '|';


procedure Register;
begin
  {$I clmcombobox.lrs}
  RegisterComponents('clm',[TclmCombobox]);
end;


{ TComboColumn }

procedure TComboColumn.Assign(Source: TPersistent);
begin
 if Source is TComboColumn then
  begin
    Color := TComboColumn(source).Color;
    Width := TComboColumn(source).Width;
    Alignment := TComboColumn(source).Alignment;
    Font.Assign(TComboColumn(source).Font);
    Visible   := TComboColumn(source).Visible;
  end;
end;

constructor TComboColumn.Create(aCollection: TCollection);
begin
  inherited;
  FFont     := TFont.Create;
  FWidth    := 100;
  FColor    := clWindow;
  FVisible  := True;
  FAlignment:= taLeftJustify;
end;

destructor TComboColumn.Destroy;
begin
  FFont.Free;
  Inherited;
end;

procedure TComboColumn.SetAlignment(const Value: tAlignment);
begin
  FAlignment := Value;
  TComboColumns(Collection).FOwner.Invalidate;
end;

procedure TComboColumn.SetColor(const Value: TColor);
begin
  FColor := Value;
  TComboColumns(Collection).FOwner.Invalidate;
end;

procedure TComboColumn.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  TComboColumns(Collection).FOwner.Invalidate;
end;

procedure TComboColumn.SetWidth(const Value: Integer);
begin
  FWidth := Value;
  TComboColumns(Collection).FOwner.Invalidate;
end;

procedure TComboColumn.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
  TComboColumns(Collection).FOwner.Invalidate;
end;


{ TComboColumns }

function TComboColumns.Add: TComboColumn;
begin
  Result := TComboColumn(inherited Add);
//  FOwner.FGrid.ColCount:= Count;
end;

constructor TComboColumns.Create(AOwner: TclmComboBox);
begin
  inherited Create(TComboColumn);
  FOwner := AOwner;
end;

function TComboColumns.GetItem(Index: Integer): TComboColumn;
begin
  Result := TComboColumn(inherited Items[index]);
end;

function TComboColumns.GetOwner: tPersistent;
begin
  Result := FOwner;
end;

function TComboColumns.Insert(index: Integer): TComboColumn;
begin
  Result := TComboColumn(inherited Insert(index));
end;

procedure TComboColumns.SetItem(Index: Integer;
  const Value: TComboColumn);
begin
  inherited SetItem(Index, Value);
end;

procedure TComboColumns.Update(Item: TCollectionItem);
begin
  inherited;
end;


{ TclmCombobox }

function TclmCombobox.GetCharWidth: integer;
begin
  Result := Canvas.TextWidth('Typical Words') div 13;
end;

function TclmCombobox.GetColumnCount: integer;
begin
  If FColumns.Count > 0 Then
    Result := FColumns.Count
  Else
    Result := FColumnWidths.Count;
end;

procedure TclmCombobox.DrawItemEvent(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  cb: TCustomComboBox;
  xl, xr: integer;
  Col         : Integer;
  ColDrawRect : TRect;
  StartForText : LongInt;
begin
  if (Index < 0) or not (Control is TCustomComboBox) then
    Exit;

  cb := TCustomComboBox(Control);
  If FColumns.Count > 0 Then
    Begin
      Col := 0;
      ColDrawRect := ARect;
      While Col < FColumns.Count Do
        begin
          case (odSelected in State) of
            False: begin
                     cb.Canvas.Brush.Color := FColumns.Items[Col].Color;
                   end;
            True:  begin
                     cb.Canvas.Brush.Color := clHighlight;
                   end;
          end; // End Case
          If FColumns.Items[Col].Visible Then
            Begin
              If Col < FColumns.Count - 1 Then
                ColDrawRect.Right := ColDrawRect.Left + FColumns.Items[Col].Width
              Else
                ColDrawRect.Right := ColDrawRect.Right + FColumns.Items[Col].Width;
              If FColumns.Items[Col].FFont <> cb.Canvas.Font Then
                Begin
                  cb.Canvas.Font    := FColumns.Items[Col].FFont;
                End;
              If FColumns.Items[Col].Alignment = taLeftJustify Then
                Begin
                  StartForText := ColDrawRect.Left + FColumnSpacing;
                End
              Else If FColumns.Items[Col].Alignment = taRightJustify Then
                Begin
                  StartForText := ColDrawRect.Right -
                    cb.Canvas.TextWidth(GetCells(Col,Index))- FColumnSpacing;
                End
              Else If FColumns.Items[Col].Alignment = taCenter Then
                Begin
                  StartForText := ColDrawRect.Left -
                   + (FColumns.Items[Col].Width div 2) - (cb.Canvas.TextWidth(GetCells(Col,Index)) div 2);
                End;
              // cb.Canvas.TextOut(xl, ARect.Top, FParser[Col]);
              // Evt. draw rectangle
              cb.Canvas.FillRect(ColDrawRect);
              cb.Canvas.TextOut(StartForText, ColDrawRect.Top, GetCells(Col,Index));
              // Could be neccesary on other platforms ? cb.Canvas.TextRect(ColDrawRect,StartForText,ColDrawRect.Top,FParser[Col]);
              if FShowColSeparators and (Col > 0) then
                begin
                  cb.Canvas.MoveTo(ColDrawRect.Left, ColDrawRect.Top);
                  cb.Canvas.LineTo(ColDrawRect.Left, ColDrawRect.Bottom);
                end;
              ColDrawRect.Left := ColDrawRect.Right;
              Col := Col + 1;
            end
          Else
            Begin
              Col := Col + 1;
            end;
        End;
    end
  Else
    Begin
      case (odSelected in State) of
        False: begin
                 cb.Canvas.Brush.Color:=Color;
                 cb.Canvas.FillRect(ARect);
               end;
        True:  begin
                 cb.Canvas.Brush.Color:=clHighlight;
                 cb.Canvas.FillRect(ARect);
               end;
      end;

      FParser.Clear;
      FParser.DelimitedText := Self.Items[Index];
      if FCharWidth = 0 then
        FCharWidth := GetCharWidth;
      for Col := 0 to FParser.Count-1 do begin
        xl:=ARect.Left + FOffsets[Col]*FCharWidth;
        If FParser[Col] <> DummyChar Then
          cb.Canvas.TextOut(xl, ARect.Top, FParser[Col]);
        if FShowColSeparators then begin
          xr:=xl - ColSeparatorMargin;
          cb.Canvas.MoveTo(xr, ARect.Top);
          cb.Canvas.LineTo(xr, ARect.Bottom);
        end;
    end;
  end;
end;

procedure TclmCombobox.TruncateText(Sender: TObject);
begin
  Text := EditText;
end;

procedure TclmCombobox.SetColumns;
var
  i: integer;

    procedure SetLengths(const s: string);
    var
      len, w: integer;
      p: integer = 0;
      oldP: integer = 1;
      col: integer = 0;
    begin
      len:=Length(s);
      while (p < len) do begin
        Inc(p);
        if (s[p]=FDelimiter) or (p=len) then begin
          w := p - oldP;
          if (p=len) then
            Inc(w);
          if (FColumnWidths.IndexOf(col) = -1) then
            FColumnWidths.Insert(col, w)
          else if (w > FColumnWidths[col]) then
            FColumnWidths[col]:=w;
          oldP:=Succ(p);
          Inc(col);
        end;
      end;
    end;

begin
  FColumnWidths.Clear;
  for i:= 0 to Items.Count-1 do
    SetLengths(Items[i]);
  for i:=0 to FColumnWidths.Count-1 do
    FColumnWidths[i]:=FColumnWidths[i]+FColumnSpacing;
  FOffsets.Clear;
  FOffsets.Insert(0, 0);
  for i:=1 to FColumnWidths.Count-1 do
    FOffsets.Insert(i, FOffsets[i-1]+FColumnWidths[i]+FColumnSpacing);

  Invalidate;
end;

procedure TclmCombobox.SetColumnSpacing(AValue: integer);
begin
  if FColumnSpacing=AValue then Exit;
  if (AValue >= 0) then
    FColumnSpacing:=AValue;
  SetColumns;
end;

procedure TclmCombobox.SetDelimiter(aDelim: AnsiChar);
begin
  if FDelimiter=aDelim then
    Exit;
  FDelimiter:=aDelim;
  FParser.Delimiter:=aDelim;
  SetColumns;
end;

procedure TclmCombobox.SetShowColSeparators(AValue: Boolean);
begin
  if FShowColSeparators=AValue then Exit;
  FShowColSeparators:=AValue;
  Invalidate;
end;

procedure TclmCombobox.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  SetColumns;
end;

class function TclmCombobox.GetControlClassDefaultSize: TSize;
begin
  Result := inherited GetControlClassDefaultSize;
  Result.cx:=180;
end;

procedure TclmCombobox.GetItems;
begin
  inherited GetItems;
  SetColumns;
end;

procedure TclmCombobox.SetItems(const Value: TStrings);
begin
  inherited SetItems(Value);
  SetColumns;
end;

procedure TclmCombobox.SetStyle(Val: TComboBoxStyle);
begin
  if Val=csOwnerDrawFixed then
    inherited SetStyle(Val);
end;

procedure TclmCombobox.SetEditColumn(Value : Integer);
begin
  FEditColumn := Value;
end;

constructor TclmCombobox.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  SetStyle(csOwnerDrawFixed);
  FColumns             := TComboColumns.Create(Self);
  FGrid                := TVirtualGrid.Create;

  FDelimiter        := #9;
  FParser           := TStringList.Create;
  FParser.Delimiter := FDelimiter;
  FColumnWidths     := TIntList.Create;
  FOffsets          := TIntList.Create;
  FColumnSpacing    := 2;
  FShowColSeparators:= False;
  OnDrawItem        := @DrawItemEvent;
  OnExit            := @TruncateText;
end;

destructor TclmCombobox.Destroy;
begin
  FreeThenNil(FGrid);
  FColumns.Free;
  FParser.Free;
  FColumnWidths.Free;
  FOffsets.FRee;
  inherited Destroy;
end;

function TclmCombobox.Getcells(aCol, aRow: Integer): string;
Var C: PCellProps;
begin
  If FColumns.Count > 0 Then
    Begin // Cells
      Result:='';
      C := FGrid.Celda[aCol,aRow];
      if C <> nil then
        Begin
          Result := C^.Text;
        end;
    end
  Else
    Begin
      Result := '';
      if (aRow >= Items.Count) then raise Exception.Create('Row index out of range');
      FParser.Clear;
      FParser.DelimitedText := Items[aRow];
      if (aCol >= FParser.Count) then raise Exception.Create('Col index out of range');
      If FParser.Strings[aCol] = DummyChar Then
        Begin
          Result := '';
        end
      Else
        Begin
          Result := FParser.Strings[aCol];
        end;
    end;
end;

procedure TclmCombobox.Setcells(aCol, aRow: Integer; const Avalue: string);
Var C: PCellProps;
    s: String;
begin
  If FColumns.Count > 0 Then
    Begin
      FGrid.ColCount := FColumns.Count;
      FGrid.RowCount := Self.Items.Count;
      // Cells
      if (aRow >= FGrid.RowCount) then raise Exception.Create('Row index out of range');
      if (aCol >= FGrid.ColCount) then raise Exception.Create('Col index out of range');
      C := FGrid.Celda[aCol,aRow];
      if C <> nil then
        begin
          if C^.Text<>nil then
            StrDispose(C^.Text);
          C^.Text := StrNew(pchar(aValue));
        end
      else
        begin
          if AValue<>'' then
            begin
              New(C);
              C^.Text := StrNew(pchar(Avalue));
              C^.Attr := nil;
              C^.Data := nil;
              FGrid.Celda[aCol,aRow]:=C;
            end;
       end;
      If aValue = '' Then
        Begin
          Self.Items[aRow] := DummyChar;
        end
      Else
        Begin
          If Self.Items.count > 0 Then
            Begin
              s := StrPas(PCellProps(FGrid.Celda[fEditColumn,aRow])^.Text);
              Self.Items[aRow] := S;
            end;
        end;
    end
  Else
    Begin
      if (aRow >= Items.Count) then raise Exception.Create('Row index out of range');
      FParser.Clear;
      FParser.DelimitedText := Items[aRow];
      if (aCol >= FParser.Count) then raise Exception.Create('Col index out of range');
      If aValue = '' Then
        Begin
          FParser.Strings[aCol] := DummyChar; // Dummy
        End
      Else
        Begin
          FParser.Strings[aCol] := aValue;
        End;
      Self.Items[aRow] := FParser.DelimitedText;
    End;
end;

procedure TclmCombobox.AddRow;
Var I : Integer;
    S : String;
begin
  If FColumns.Count > 0 Then
    Begin
      FGrid.RowCount := FGrid.RowCount + 1;
      Self.Items.Add(DummyChar);
    end
  Else
    Begin
      S := '';
      For I := 0 to FColumns.Count - 1 do
        Begin
          S := S + DummyChar + FDelimiter;
        end;
      Self.Items.Add(S);
    end;
end;

function TclmCombobox.IndexInColumnOf(acol : Integer; s : string): Integer;
Var Stop : Boolean;
    I    : Integer;
Begin
  if (aCol >= FGrid.ColCount) then raise Exception.Create('IndexColumnOf: col out of range');
  Stop := False;
  I    := 0;
  While not Stop do
    Begin
      If GetCells(aCol,I) = s Then
        Stop := True
      Else
        Inc(I);
    end;
  If Stop Then
    Result := I
  Else
    Result := -1;
end;

function TclmCombobox.IndexInRowOf(aRow : Integer; s : string): Integer;
Var Stop : Boolean;
    I    : Integer;
Begin
  if (aRow >= FGrid.RowCount) then raise Exception.Create('IndexinRowOf: row out of range');
  Stop := False;
  I    := 0;
  While not Stop do
    Begin
      If GetCells(I,aRow) = s Then
        Stop := True
      Else
        Inc(I);
    end;
  If Stop Then
    Result := I
  Else
    Result := -1;
end;

function TclmCombobox.GetEditText : string;
begin
  if (EditColumn >= 0) and (ItemIndex >= 0) then
    Begin
      If FColumns.Count > 0 Then
        Begin
          if (EditColumn >= FGrid.ColCount) then raise Exception.Create('EditColumn out of range');
          Result := GetCells(EditColumn,ItemIndex);
        end
      Else
        Begin
          FParser.Clear;
          FParser.DelimitedText := Self.Items[ItemIndex];
          if (EditColumn >= FParser.Count) then raise Exception.Create('EditColumn out of range');
          If FParser.Strings[EditColumn] = DummyChar Then
            Begin
              Result := '';
            End
          Else
            Begin
              Result := FParser.Strings[EditColumn];
            End;
        end;
    end
  else
    Begin
      Result := Text;
    end;
end;

(*// Only needed to be able to change editcolumn; item to be seen aften leaving combo
Procedure TclmCombobox.NoNils;
Var aCol : Integer;
    aRow : Integer;
    C : PCellProps;
Begin
  For aRow := 0 to FGrid.RowCount - 1 Do
    For aCol := 0 to FGrid.ColCount - 1 Do
      Begin
        C := FGrid.Celda[aCol,aRow];
        if C = nil then
          begin
            New(C);
            C^.Text := StrNew(pchar(''));
            C^.Attr := nil;
            C^.Data := nil;
            FGrid.Celda[aCol,aRow]:=C;
          end;
      end;
end;*)

Procedure TclmCombobox.Clear;
Begin
  Inherited;
  FGrid.Clear;
  FColumns.Clear;
end;


initialization
end.

