% node(NodeId, SoftwareCapabilities, HardwareCapabilities, IoTCapabilities).
node(privateCloud,[ubuntu, mySQL, python], 128, [gpu]).
node(accesspoint,[ubuntu, mySQL, python], 4, [lightshub, videocamera]).
node(edgenode,[ubuntu, python], 8, [gpu, lightshub, videocamera]).

% link(NodeId1, NodeId2, FeaturedLatency, FeaturedBandwidth).
link(privateCloud, accesspoint, 5, 1000).
link(accesspoint, privateCloud, 5, 1000).
link(accesspoint, edgenode, 5, 20).
link(edgenode, accesspoint, 5, 20).
link(privateCloud, edgenode, 15, 18).
link(edgenode, privateCloud, 15, 18).