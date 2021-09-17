% SockShop Description
% See: https://raw.githubusercontent.com/microservices-demo/microservices-demo.github.io/0ac7e0e579d83ce04e14f3b0942f7a463b72da74/assets/Architecture.png

application(sockshop, [
    frontend, 
    order, 
    payment, 
    user, 
    catalogue, 
    cart, 
    shipping, 
    queue, 
    queuemaster
    ]).

% java8 128MB https://www.java.com/en/download/help/sysreq.html
% nodejs 256MB https://stackoverflow.com/questions/9613528/node-js-with-v8-suitable-for-limited-memory-device
% rabbitmq 256MB https://www.rabbitmq.com/production-checklist.html#:~:text=Nodes%20hosting%20RabbitMQ%20should%20have,The%20recommended%20vm_memory_high_watermark.
% go 128MB ? https://coralogix.com/blog/optimizing-a-golang-service-to-reduce-over-40-cpu/ [too much 2000 jetbrains.com/help/go/installation-guide.html#requirements]
% mongodb 512MB (1GB per 100.000 assets) https://learn.fotoware.com/On-Premises/FotoWeb/05_Configuring_sites/Setting_the_MongoDB_instance_that_FotoWeb_uses/MongoDB_disk_and_memory_requirements#:~:text=MongoDB%20requires%20approximately%201GB%20of,performance%2C%20and%20should%20be%20avoided.
% mySQL 512MB https://dev.mysql.com/doc/refman/8.0/en/memory-use.html#:~:text=The%20default%20configuration%20is%20designed,and%20buffer%2Drelated%20system%20variables.
% dotnet 512MB https://docs.microsoft.com/en-us/dotnet/framework/get-started/system-requirements

service(frontend, [nodejs], 256, [user]).
service(order, [java, dotnet, mongodb], 1152, []).
service(payment, [go], 128, []).
service(user, [go, mongodb], 640, []).
service(catalogue, [go, mySQL], 640, []).
service(cart, [java, mongodb], 640, []).
service(shipping, [java], 128, []).
service(queue, [rabbitmq], 256, []).
service(queuemaster, [java], 128, []).
/*
s2s(frontend, order, 150, 10).
s2s(order, frontend, 150, 10).

s2s(frontend, payment, 200, 2).
s2s(payment, frontend, 200, 2).

s2s(frontend, user, 150, 5).
s2s(user, frontend, 150, 5).

s2s(frontend, catalogue, 150, 5).
s2s(catalogue, frontend, 150, 5).

s2s(frontend, cart, 250, 10).
s2s(cart, frontend, 250, 10).

s2s(order, shipping, 150, 10).
s2s(shipping, queue, 150, 10).

s2s(queue, queuemaster, 150, 10).
s2s(queuemaster, queue, 150, 10).
*/

s2s(frontend, order, 150, 5).
s2s(order, frontend, 150, 5).

s2s(frontend, payment, 200, 1).
s2s(payment, frontend, 200, 1).

s2s(frontend, user, 150, 1).
s2s(user, frontend, 150, 1).

s2s(frontend, catalogue, 150, 1).
s2s(catalogue, frontend, 150, 1).

s2s(frontend, cart, 250, 5).
s2s(cart, frontend, 250, 5).

s2s(order, shipping, 150, 5).
s2s(shipping, queue, 150, 5).

s2s(queue, queuemaster, 150, 5).
s2s(queuemaster, queue, 150, 5).
