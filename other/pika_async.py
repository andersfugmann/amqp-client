# -*- coding: utf-8 -*-

import logging
import pika

LOG_FORMAT = ('%(levelname) -10s %(asctime)s %(name) -30s %(funcName) '
              '-35s %(lineno) -5d: %(message)s')
LOGGER = logging.getLogger(__name__)


class ExampleConsumer(object):
    QUEUE = 'pika.test'

    def __init__(self, amqp_url):
        self._connection = None
        self._channel = None
        self._closing = False
        self._consumer_tag = None
        self._url = amqp_url

    def connect(self):
        LOGGER.info('Connecting to %s', self._url)
        return pika.SelectConnection(pika.URLParameters(self._url),
                                     self.on_connection_open,
                                     stop_ioloop_on_close=False)

    def on_connection_open(self, unused_connection):
        LOGGER.info('Connection opened')
        self.open_channel()

    def open_channel(self):
        LOGGER.info('Creating a new channel')
        self._connection.channel(on_open_callback=self.on_channel_open)

    def on_channel_open(self, channel):
        LOGGER.info('Channel opened')
        self._channel = channel
        self.setup_queue(self.QUEUE)

    def setup_queue(self, queue_name):
        self._channel.queue_declare(self.on_queue_declareok, queue_name, auto_delete = True)

    def on_queue_declareok(self, method_frame):
        self.start_consuming()

    def start_consuming(self):
        LOGGER.info('Issuing consumer related RPC commands')
        self._consumer_tag = self._channel.basic_consume(self.on_message,
                                                         self.QUEUE,
                                                         no_ack = True)
        self.produce(100000)

    def produce(self, n):

        properties = pika.BasicProperties()

        self._channel.basic_publish("", self.QUEUE,
                                    str(n),
                                    properties)
        if n > 0:
            self._connection.add_timeout(-1, lambda: self.produce(n-1))
        else:
            print "Done producing"

    def on_message(self, unused_channel, basic_deliver, properties, body):
        n = int(body)
        if (n % 1000) == 0:
            print n
        if n == 0:
            self._connection.ioloop.stop()

    def run(self):
        self._connection = self.connect()
        self._connection.ioloop.start()

    def stop(self):
        LOGGER.info('Stopping')
        self._closing = True
        self.stop_consuming()
        self._connection.ioloop.start()
        LOGGER.info('Stopped')

    def close_connection(self):
        LOGGER.info('Closing connection')
        self._connection.close()


def main():
    logging.basicConfig(level=logging.INFO, format=LOG_FORMAT)
    example = ExampleConsumer('amqp://guest:guest@localhost:5672/%2F')
    try:
        example.run()
    except KeyboardInterrupt:
        example.stop()


if __name__ == '__main__':
    main()
