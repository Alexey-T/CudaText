unit Plugins.Utils;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils;

type
  TStringPull<T>=class
    type
      PNode=^TNode; 
      PItem=^TItem;
      TNode=bitpacked record
        Items:PItem;
        ItemsCount:byte;
        Data:T;
        IsEnd:boolean;
      end;     
      TItem=packed record
        Sym:Char;
        Node:PNode;
      end;
    var
      root:PNode;
    constructor Create();
    procedure Add(const str:String;Data:T);
    procedure Del(const str:String);
    function Get(const str:String;out Data:T):boolean;
    function IsIn(const str:String):boolean;  
    procedure Clear();
    destructor Destroy; override;
    //Define if T need finalization
    procedure ClearCallBack(var Data:T);virtual;
  end;

implementation

//TStringPull<T>

constructor TStringPull<T>.Create();
begin
  root:=GetMem(SizeOf(TStringPull<T>));
  root.Items:=nil;
  root.ItemsCount:=0;
  root.IsEnd:=False;
end;

procedure TStringPull<T>.Add(const str:String;Data:T);
  function InsertNode(LastData:PItem;Pos,LastSize:byte;Sym:Char;Node:Pointer):PItem;inline;
  var
    i:byte;
  begin       
    Result:=GetMem(SizeOf(TItem)*(LastSize+1));
    for i:=1 to Pos do
    begin
      Result^:=LastData^;
      inc(LastData);  
      inc(Result);
    end;     
    Result^.Node:=Node;   
    Result^.Sym:=Sym;
    inc(Result);
    for i:=Pos+1 to LastSize do  
    begin    
      Result^:=LastData^;
      inc(LastData);
      inc(Result);
    end;
    FreeMem(LastData);
  end;
var
  c:char;
  i0:byte;
  CurrItem:PItem;
  CurrNode,NewNode:PNode;
begin
  CurrNode:=root;
  for c in str do
    if Assigned(CurrNode^.Items)then
    begin
      i0:=CurrNode^.ItemsCount;
      CurrItem:=CurrNode^.Items;

      while(i0>0)and(CurrItem.Sym>=c)do
      begin
        if CurrItem.Sym=c then
          //Found    
          CurrNode:=CurrItem^.Node;
        inc(CurrItem);
        dec(i0);
      end;
      //Not found
      NewNode:=GetMem(SizeOf(TStringPull<T>));
      CurrNode^.Items:=InsertNode(CurrNode^.Items,i0,CurrNode^.ItemsCount,c,NewNode);
      inc(CurrNode^.ItemsCount); 
      CurrNode:=NewNode;      
      CurrNode^.Items:=nil;
      CurrNode^.ItemsCount:=0;
      CurrNode^.IsEnd:=False;
    end
    else
    begin
      //No Children
      CurrNode^.Items:=GetMem(SizeOf(TItem));
      CurrNode^.ItemsCount:=1;
      CurrNode^.Items^.Node:=GetMem(SizeOf(TStringPull<T>));
      CurrNode^.Items^.Sym:=c;
      CurrNode:=CurrNode^.Items^.Node;
      CurrNode^.Items:=nil;
      CurrNode^.ItemsCount:=0;
      CurrNode^.IsEnd:=False;
    end;
  if CurrNode^.IsEnd then
    ClearCallBack(CurrNode^.Data);
  CurrNode^.IsEnd:=True;
  CurrNode^.Data:=Data;
end;

procedure TStringPull<T>.Del(const str:String);
  function ProcessNode(Node:PNode;Step:integer):Boolean;
    function RmNode(LastData:PItem;Pos,LastSize:byte):PItem;inline;
    var
      i:byte;
    begin
      Result:=GetMem(SizeOf(TItem)*(LastSize-1));
      for i:=1 to Pos-1 do
      begin
        Result^:=LastData^;
        inc(LastData);
        inc(Result);
      end;      
      inc(LastData);
      for i:=Pos+1 to LastSize do
      begin
        Result^:=LastData^;
        inc(LastData);
        inc(Result);
      end;
      FreeMem(LastData);
    end;
  var
    i0:byte;
    CurrItem:PItem;
  begin
    if Step>Length(str) then
    begin            
      if Node^.IsEnd then
        ClearCallBack(Node^.Data);   
      Node^.IsEnd:=False;
      Result:=not Assigned(Node^.Items);
      if Result then
        FreeMem(Node);
    end
    else
      if Assigned(Node^.Items)then
      begin
        i0:=Node^.ItemsCount;
        CurrItem:=Node^.Items;

        while(i0>0)and(CurrItem.Sym>=str[Step])do
        begin
          if CurrItem^.Sym=str[Step] then
            //Found
            if ProcessNode(CurrItem^.Node,Step+1) then
            begin
              dec(Node^.ItemsCount);
              if Node^.ItemsCount=0 then
              begin
                FreeMem(Node^.Items);
                Node^.Items:=nil;
                if not Node^.IsEnd then
                  FreeMem(Node);
                exit(not Node^.IsEnd);
              end
              else
              begin
                Node^.Items:=RmNode(Node^.Items,i0,Node^.ItemsCount);
                exit(false);
              end;
            end
            else
              exit(false);
          inc(CurrItem);
          dec(i0);
        end;
        //Not found
        Result:=False;
      end
      else
        //No Children   
        Result:=False;
  end;
begin
  ProcessNode(root,1);
end;

function TStringPull<T>.Get(const str:String;out Data:T):boolean;
var
  c:char;
  i0:byte;
  CurrItem:PItem;
  CurrNode:PNode;
begin
  CurrNode:=root;
  for c in str do
    if Assigned(CurrNode^.Items)then
    begin
      i0:=CurrNode^.ItemsCount;
      CurrItem:=CurrNode^.Items;

      while(i0>0)and(CurrItem.Sym>=c)do
      begin
        if CurrItem.Sym=c then
          //Found
          CurrNode:=CurrItem^.Node;
        inc(CurrItem);
        dec(i0);
      end;
      //Not found
      exit(false);
    end
    else
      //No Children   
      exit(false);
  if CurrNode^.IsEnd then
    Data:=CurrNode^.Data;
  Result:=CurrNode^.IsEnd;
end;

function TStringPull<T>.IsIn(const str:String):boolean;
var
  c:char;
  i0:byte;
  CurrItem:PItem;
  CurrNode:PNode;
begin
  CurrNode:=root;
  for c in str do
    if Assigned(CurrNode^.Items)then
    begin
      i0:=CurrNode^.ItemsCount;
      CurrItem:=CurrNode^.Items;

      while(i0>0)and(CurrItem.Sym>=c)do
      begin
        if CurrItem.Sym=c then
          //Found
          CurrNode:=CurrItem^.Node;
        inc(CurrItem);
        dec(i0);
      end;
      //Not found
      exit(false);
    end
    else
      //No Children
      exit(false);
  Result:=CurrNode^.IsEnd;
end;  

procedure TStringPull<T>.Clear();
  function ProcessNode(Node:PNode):Boolean;
  var
    CurrItem:PItem;
  begin
    if Assigned(Node^.Items)then
    begin
      CurrItem:=Node^.Items;

      while Node^.ItemsCount>0 do
      begin
        ProcessNode(CurrItem^.Node);
        inc(CurrItem);
        dec(Node^.ItemsCount);
      end;
      //Not found
      FreeMem(Node^.Items);
    end;
    if Node^.IsEnd then
      ClearCallBack(Node^.Data);
    FreeMem(Node);
  end;
begin
  ProcessNode(root);
end;


destructor TStringPull<T>.Destroy;
begin
  Clear();
end;

procedure TStringPull<T>.ClearCallBack(var Data:T);
begin
end;

end.

