program test;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes
  { you can add units after this },xxhash,error_private,zstd,zstd_internal,zstd_common,entropy_common,algoutil
  ,cover,divsufsort,divsufsortutils,sysutils,zstd_compressf,ZSTD_DECOMPRESSf;
function compress(infilename,outfilename:string):integer;
var
  infile,outfile:integer;
  f:file of byte;
  fsize:int64;
  inbuffer,cbuffer:pbyte;
  i:integer;
  cBuffSize,cSize:int32;
begin
  assignfile(f,infilename);
  reset(f);
  fsize:=filesize(f);
  closefile(f);
  inbuffer:=allocmem(fsize);
  infile:=FileOpen(infilename,fmOpenRead);
  if infile=-1 then
    exit(-1);
  i:=FileRead(infile,inbuffer^,fsize);
  fileclose(infile);
  cBuffSize:=ZSTD_compressBound(fsize);
  cbuffer:=allocmem(cBuffSize);
  cSize := ZSTD_compress(cbuffer, cBuffSize, inbuffer, fSize, 1);
  if ZSTD_isError(cSize)<>0 then
  begin
    writeln(stderr,ZSTD_getErrorName(cSize));
    freemem(inbuffer);
    freemem(cbuffer);
    exit(-3);
  end;
  outfile:=FileCreate(outfilename,fmShareDenyWrite);
  if outfile=-1 then
     exit(-2);
  i:=filewrite(outfile,cbuffer^,cSize);
  if i<>cSize then
     exit(-4);
  fileclose(outfile);
end;
function decompress(infilename,outfilename:string):integer;
var
  infile,outfile:integer;
  f:file of byte;
  cSize,rsize:int64;
  inbuffer,rbuffer:pbyte;
  i:integer;
  rBuffSize,dSize:int32;
begin
  assignfile(f,infilename);
  reset(f);
  cSize:=filesize(f);
  closefile(f);
  inbuffer:=allocmem(cSize);
  infile:=FileOpen(infilename,fmOpenRead);
  if infile=-1 then
    exit(-1);
  i:=FileRead(infile,inbuffer^,cSize);
  fileclose(infile);
  rSize:=ZSTD_getFrameContentSize(inbuffer,cSize);
  if rSize= ZSTD_CONTENTSIZE_ERROR then
  begin
    writeln(stderr,infilename,' is not compressed by zstd!');
    exit(-5);
  end;
  if rSize= ZSTD_CONTENTSIZE_UNKNOWN then
  begin
    writeln(stderr,infilename,' original size unknown!');
    exit(-6);
  end;
  rbuffer:=allocmem(rSize);
  dSize := ZSTD_decompress(rbuffer, rSize, inbuffer, cSize);
  if ZSTD_isError(cSize)<>0 then
  begin
    writeln(stderr,ZSTD_getErrorName(cSize));
    freemem(inbuffer);
    freemem(rbuffer);
    exit(-3);
  end;
  if dSize <> rSize then
  begin
    writeln(stderr,' Impossible because zstd will check this condition!');
    exit(-7);
  end;
  outfile:=FileCreate(outfilename,fmShareDenyWrite);
  if outfile=-1 then
     exit(-2);
  i:=filewrite(outfile,rbuffer^,rSize);
  if i<>cSize then
     exit(-4);
  fileclose(outfile);
end;
begin

end.

