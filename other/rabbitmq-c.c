/* vim:set ft=c ts=2 sw=2 sts=2 et cindent: */
/*
 * ***** BEGIN LICENSE BLOCK *****
 * Version: MIT
 *
 * Portions created by Alan Antonuk are Copyright (c) 2012-2013
 * Alan Antonuk. All Rights Reserved.
 *
 * Portions created by VMware are Copyright (c) 2007-2012 VMware, Inc.
 * All Rights Reserved.
 *
 * Portions created by Tony Garnock-Jones are Copyright (c) 2009-2010
 * VMware, Inc. and Tony Garnock-Jones. All Rights Reserved.
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use, copy,
 * modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 * ***** END LICENSE BLOCK *****
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <stdint.h>
#include <amqp_tcp_socket.h>
#include <amqp.h>
#include <amqp_framing.h>

static void send_batch(amqp_connection_state_t conn,
                       char const *queue_name,
                       int message_count)
{
  int i;

  char message[256];
  amqp_bytes_t message_bytes;

  for (i = 0; i < (int)sizeof(message); i++) {
    message[i] = i & 0xff;
  }

  message_bytes.len = sizeof(message);
  message_bytes.bytes = message;

  for (i = 0; i < message_count; i++) {

    amqp_basic_publish(conn,
                       1,
                       amqp_cstring_bytes(""),
                       amqp_cstring_bytes(queue_name),
                       0,
                       0,
                       NULL,
                       message_bytes);
    if (i % 1000 == 0) {
      printf("Sent 1000\n");
    }
  }
}

static void recv_batch(amqp_connection_state_t conn,
                       char const *queue_name,
                       int message_count)
{
  for (int i = 0; i < message_count; i++) {
   amqp_basic_get(conn,
                  1,
                  amqp_cstring_bytes(queue_name),
                  1);
   if (i % 1000 == 0) {
     printf("Receive 1000\n");
   }
  }
}


int main(int argc, char const *const *argv)
{
  char const *hostname;
  int port, status;
  int message_count;
  amqp_socket_t *socket = NULL;
  amqp_connection_state_t conn;

  if (argc < 4) {
    fprintf(stderr, "Usage: amqp_producer host port message_count\n");
    return 1;
  }

  hostname = argv[1];
  port = atoi(argv[2]);
  message_count = atoi(argv[3]);

  conn = amqp_new_connection();

  socket = amqp_tcp_socket_new(conn);
  status = amqp_socket_open(socket, hostname, port);
  amqp_login(conn, "/", 0, 131072, 0, AMQP_SASL_METHOD_PLAIN, "guest", "guest");
  amqp_channel_open(conn, 1);
  amqp_get_rpc_reply(conn);

  send_batch(conn, "test_queue", message_count);
  recv_batch(conn, "test_queue", message_count);

  amqp_channel_close(conn, 1, AMQP_REPLY_SUCCESS);
  amqp_connection_close(conn, AMQP_REPLY_SUCCESS);
  amqp_destroy_connection(conn);
  return 0;
}
