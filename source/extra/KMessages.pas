unit KMessages;

{$include KControls.inc}
{$WEAKPACKAGEUNIT ON}

interface

{ WM_POWERBROADCAST flags}
const
  PBT_APMQUERYSUSPEND           = $0000;
  PBT_APMQUERYSTANDBY           = $0001;

  PBT_APMQUERYSUSPENDFAILED     = $0002;
  PBT_APMQUERYSTANDBYFAILED     = $0003;

  PBT_APMSUSPEND                = $0004;
  PBT_APMSTANDBY                = $0005;

  PBT_APMRESUMECRITICAL         = $0006;
  PBT_APMRESUMESUSPEND          = $0007;
  PBT_APMRESUMESTANDBY          = $0008;

  PBTF_APMRESUMEFROMFAILURE     = $00000001;

  PBT_APMBATTERYLOW             = $0009;
  PBT_APMPOWERSTATUSCHANGE      = $000A;

  PBT_APMOEMEVENT               = $000B;
  PBT_APMRESUMEAUTOMATIC        = $0012;


implementation

end.
