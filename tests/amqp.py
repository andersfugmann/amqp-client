import pika

connection = pika.BlockingConnection()
channel = connection.channel()

# Get ten messages and break out
i = 1
while True:
    channel.basic_publish("", "anders", "message: " + str(i))
    meth, _, data = channel.basic_get("anders")
    channel.basic_ack(meth.delivery_tag)
    print "Received: " + data
    i = i + 1
