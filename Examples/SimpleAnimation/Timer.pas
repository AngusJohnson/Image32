unit Timer;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  1.0                                                             *
* Date      :  20 August 2019                                                  *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2019                                         *
* Purpose   :  A much more accurate and responsive timer than Delphi's         *
*           :  TTimer control, especially with intervals below 10 msec.        *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

uses
  Windows, SysUtils, Classes, Math, Forms;

{$WARN SYMBOL_PLATFORM OFF}

type
  TTimerThread = class(TThread)
  private
    fInterval     : double;
    fCancelEvent  : THandle;
    fOnTimer      : TNotifyEvent;
    procedure DoOnTimer;
  protected
    procedure Execute; override;
    procedure Terminate;
  public
    constructor Create(interval: double; priority: TThreadPriority;
      onTimer, onTerminated: TNotifyEvent);
    destructor Destroy; override;
    property Interval: double write fInterval;
  end;

  TTimerEx = class
  private
    fTimerThread : TTimerThread;
    fInterval    : double;
    fOnTimer     : TNotifyEvent;
    fPriority    : TThreadPriority;
    function GetEnabled: Boolean;
    procedure OnTerminated(Sender: TObject);
    procedure SetEnabled(value: Boolean);
    procedure SetInterval(value: double);
  public
    constructor Create;
    destructor Destroy; override;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Interval: double read fInterval write SetInterval;
    property Priority: TThreadPriority read fPriority write fPriority;
    property OnTimer: TNotifyEvent read fOnTimer write fOnTimer;
  end;

implementation

//------------------------------------------------------------------------------
// TTimerThread
//------------------------------------------------------------------------------

constructor TTimerThread.Create(interval: double; priority:
  TThreadPriority; onTimer, onTerminated: TNotifyEvent);
begin
  FreeOnTerminate := true;
  OnTerminate := onTerminated;
  fInterval := interval;
  fOnTimer := onTimer;
  fCancelEvent := Windows.CreateEvent(nil, true, false, nil);
  inherited Create(false);
  self.Priority := priority;
end;
//------------------------------------------------------------------------------

destructor TTimerThread.Destroy;
begin
  inherited Destroy;
  CloseHandle(fCancelEvent);
end;
//------------------------------------------------------------------------------

procedure TTimerThread.DoOnTimer;
begin
  //nb: this method is synchronized
  if not Terminated then fOnTimer(Self);
end;
//------------------------------------------------------------------------------

procedure TTimerThread.Execute;
var
  delay, prevDelay, f: double;
  freq, cnt1, cnt2 : TLargeInteger;
  interval: DWORD;
begin
  prevDelay := fInterval;
  QueryPerformanceFrequency(freq);
  f := 1000/freq;
  QueryPerformanceCounter(cnt1);
  while not Terminated do
  begin
    QueryPerformanceCounter(cnt2);
    delay := (((cnt2 - cnt1) * f - fInterval) + prevDelay);
    cnt1 := cnt2;
    prevDelay := delay;
    if delay >= fInterval -1 then
      interval := 1 else
      interval := Round(fInterval - delay);
    if WaitForSingleObject(fCancelEvent, interval) = WAIT_OBJECT_0 then
      Break;
    Synchronize(DoOnTimer);
  end;
end;
//------------------------------------------------------------------------------

procedure TTimerThread.Terminate;
begin
  SetEvent(fCancelEvent);
  inherited Terminate;
end;

//------------------------------------------------------------------------------
// TTimerEx
//------------------------------------------------------------------------------

constructor TTimerEx.Create;
begin
  fInterval := 1000; //msec
  fPriority := tpLowest;
end;
//------------------------------------------------------------------------------

destructor TTimerEx.Destroy;
begin
  Enabled := false;

  //nb: If this object is being destroyed on application shutdown while a
  //thread is still executing, for example freeing TTimerEx in the main form's
  //destructor, then signaling thread termination here (Enabled := false;)
  //won't necessarily ensure termination before the OS cleans up. If it's
  //essential that the thread terminates normally (or if memory leak reporting
  //annoys) then call Application.ProcessMessages after this object is freed.
  //Alternatively, include Forms in 'uses' and call ProcessMessages here.
  while Enabled do Application.ProcessMessages;

  inherited Destroy;
end;
//------------------------------------------------------------------------------

procedure TTimerEx.OnTerminated(Sender: TObject);
begin
  fTimerThread := nil;
end;
//------------------------------------------------------------------------------

function TTimerEx.GetEnabled: Boolean;
begin
  Result := Assigned(fTimerThread);
end;
//------------------------------------------------------------------------------

procedure TTimerEx.SetEnabled(value: Boolean);
begin
  if value = GetEnabled then Exit;
  if not value then
    fTimerThread.Terminate
  else if (fInterval > 0) and Assigned(fOnTimer) then
    fTimerThread := TTimerThread.Create(fInterval,
      fPriority, fOnTimer, OnTerminated);
end;
//------------------------------------------------------------------------------

procedure TTimerEx.SetInterval(value: double);
begin
  fInterval := value;
  if Assigned(fTimerThread) then
    fTimerThread.Interval := value;
end;
//------------------------------------------------------------------------------

end.
