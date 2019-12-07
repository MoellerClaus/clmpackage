unit DemoclmCombo;

// Demo for a Columncombobox in Lazarus
// Documentation in seperate file
// Claus Moeller 01.2014, vig.hjaelp@gmail.com

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, ComCtrls, DbCtrls, clmCombobox, clmDBLabeledEdit;

type

  { TComboDemoForm }

  TComboDemoForm = class(TForm)
    Button1: TButton;
    CheckHide1: TCheckBox;
    CheckJustify: TCheckBox;
    CheckLines: TCheckBox;
    clmCombobox1: TclmCombobox;
    clmDBLabeledEdit1: TclmDBLabeledEdit;
    Color1Btn: TButton;
    DBNavigator1: TDBNavigator;
    FontButton: TButton;
    IndexOgBtn: TButton;
    Memo1: TMemo;
    NotAdvanced: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    ColorDialog1: TColorDialog;
    FontDialog1: TFontDialog;
    TextBtn: TButton;
    procedure Button1Click(Sender: TObject);
    procedure clmCombobox1Change(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure TextBtnClick(Sender: TObject);
    procedure CheckHide1Click(Sender: TObject);
    procedure CheckJustifyClick(Sender: TObject);
    procedure CheckLinesChange(Sender: TObject);
    procedure Color1BtnClick(Sender: TObject);
    procedure FontButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure IndexOgBtnClick(Sender: TObject);
    procedure NotAdvancedClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    // properties from TComboBox
  end;

var
  ComboDemoForm: TComboDemoForm;

implementation


{$R *.lfm}

{ TComboDemoForm }

procedure TComboDemoForm.FormActivate(Sender: TObject);
begin
  Button1Click(Sender);
  clmCombobox1.ItemIndex:=0;
end;

procedure TComboDemoForm.IndexOgBtnClick(Sender: TObject);
begin
  ShowMessage('Index of Claus in columns 1 is : ' +
    IntToStr(clmCombobox1.IndexInColumnOf(0,'Claus')));
end;


procedure TComboDemoForm.Button1Click(Sender: TObject);
begin
  clmCombobox1.clear;
//  clmComboBox1.EditColumn:=0;
  clmCombobox1.Columns.add;
  clmCombobox1.Columns.Add;
  clmCombobox1.Columns.Add;
  clmCombobox1.Columns.Items[0].Width:=150;
  clmCombobox1.Columns.Items[0].Color:=$00A6FFFF;
//  clmCombobox1.Columns.Items[0].Visible:=False;
//  ShowMessage(IntToSTr(clmCombobox1.Columns.Count));
  clmCombobox1.AddRow;
  clmCombobox1.AddRow;
  clmCombobox1.AddRow;
//  ShowMessage(IntToStr(clmCombobox1.Columns.count));
  clmCombobox1.Cells[0,0] := 'Life';
  clmCombobox1.Cells[1,0] := '';
  clmCombobox1.Cells[2,0] := 'Empty';
  clmCombobox1.Cells[0,1] := 'Claus';
  clmCombobox1.Cells[1,1] := 'Whisky';
  clmCombobox1.Cells[2,1] := 'Lazarus';
  clmCombobox1.Cells[0,2] := '0.2';
  clmCombobox1.Cells[1,2] := '1.2';
  clmCombobox1.Cells[2,2] := '2.2';
end;

procedure TComboDemoForm.clmCombobox1Change(Sender: TObject);
begin

end;

procedure TComboDemoForm.Memo1Change(Sender: TObject);
begin

end;


procedure TComboDemoForm.TextBtnClick(Sender: TObject);
begin
  ShowMessage('Text is: ' + clmCombobox1.Text);
end;

procedure TComboDemoForm.CheckHide1Click(Sender: TObject);
begin
  If CheckHide1.Checked Then
    clmCombobox1.Columns.Items[1].Visible:=False
  Else
    clmCombobox1.Columns.Items[1].Visible:=True;
end;

procedure TComboDemoForm.CheckJustifyClick(Sender: TObject);
begin
  If CheckJustify.checked Then
    clmCombobox1.Columns.Items[0].Alignment := taRightJustify
  Else
    clmCombobox1.Columns.Items[0].Alignment := taLeftJustify;
end;

procedure TComboDemoForm.CheckLinesChange(Sender: TObject);
begin
  If CheckLines.checked Then
    clmCombobox1.ShowColSeparators := True
  Else
    clmCombobox1.ShowColSeparators := False;
end;

procedure TComboDemoForm.Color1BtnClick(Sender: TObject);
begin
  ColorDialog1.Color:=clmCombobox1.Columns.Items[1].Color;
  ColorDialog1.Execute;
  clmCombobox1.Columns.Items[1].Color := ColorDialog1.Color;
end;

procedure TComboDemoForm.FontButtonClick(Sender: TObject);
begin
  FontDialog1.Font := clmCombobox1.Columns.Items[2].Font;
  FontDialog1.Execute;
  clmCombobox1.Columns.Items[2].Font := FontDialog1.Font;
end;

procedure TComboDemoForm.NotAdvancedClick(Sender: TObject);
begin
  clmcombobox1.Columns.Clear;
  clmcombobox1.ColumnSpacing := 8;
  clmCombobox1.Items.Clear;
  clmCombobox1.Items.Add('1:Denmark' + #9 + 'wonderful' + #9 + 'country');
  clmCombobox1.Items.Add('Claus' + #9 + 'Møller' + #9 + 'Hejsan');
  clmCombobox1.Items.Add('Lazarus' + #9 + 'is' + #9 + 'Good');
end;


end.

