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
  Windows, SysUtils, Classes, Math;

{$WARN SYMBOL_PLATFORM OFF}

type
  TTimerThread = class(TThread)
  private
    fInterval    : double;
    fCancelEvent : THandle;
    fOnTimer     : TNotifyEvent;
    procedure DoOnTimer;
  protected
    procedure Terminate;
    procedure Execute; override;
  public
    constructor Create(interval: double;
      priority: TThreadPriority; onTimerEvent: TNotifyEvent);
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

constructor TTimerThread.Create(interval: double;
  priority: TThreadPriority; onTimerEvent: TNotifyEvent);
begin
  FreeOnTerminate := false;
  fInterval := interval;
  fOnTimer := onTimerEvent;
  fCancelEvent := Windows.CreateEvent(nil, true, false, nil);
  inherited Create(False);
  self.Priority := priority;
end;
//------------------------------------------------------------------------------

destructor TTimerThread.Destroy;
begin
  CloseHandle(fCancelEvent);
  inherited Destroy;
end;
//------------------------------------------------------------------------------

procedure TTimerThread.DoOnTimer;
begin
  if not Terminated then fOnTimer(Self);
end;
//------------------------------------------------------------------------------

procedure TTimerThread.Execute;
var
  delay, prevDelay, f: double;
  freq, cnt1, cnt2 : TLargeInteger;
  interval: DWORD;
begin
  delay := 0;
  prevDelay := fInterval;
  interval := Round(fInterval);
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
  SetEvent(fCancelEvent); //signals cancel
  inherited Terminate;
end;

//------------------------------------------------------------------------------
// TTimerEx
//------------------------------------------------------------------------------

constructor TTimerEx.Create;
begin
  fInterval := 1000; //default = 1 second
  fPriority := tpLowest;
end;
//------------------------------------------------------------------------------

destructor TTimerEx.Destroy;
begin
  Enabled := false;
  inherited Destroy;
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
  if value then
  begin
    if (fInterval > 0) and Assigned(fOnTimer) then
      fTimerThread := TTimerThread.Create(fInterval, fPriority, fOnTimer);
  end else
  begin
    fTimerThread.Terminate;
    FreeAndNil(fTimerThread);
  end;
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
