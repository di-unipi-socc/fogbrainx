% application(AppId, [ServiceIds]).
application(lightsApp, [mlOptimiser, lightsDriver]).
% service(ServiceId, [SoftwareRequirements], HardwareRequirements, IoTRequirements).
service(mlOptimiser, [mySQL, python, ubuntu], 16, [gpu]).
service(lightsDriver, [ubuntu], 2, [videocamera, lightshub]).
% s2s(ServiceId1, ServiceId2, MaxLatency, MinBandwidth)
s2s(mlOptimiser, lightsDriver, 50, 0.5).
s2s(lightsDriver, mlOptimiser, 20, 16).
