/* Copyright LiChunhui (chunhui_true@163.com) 
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); 
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at 
 *
 *   http://www.apache.org/licenses/LICENSE-2.0 
 *
 * Unless required by applicable law or agreed to in writing, software 
 * distributed under the License is distributed on an "AS IS" BASIS, 
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
 * See the License for the specific language governing permissions and 
 * limitations under the License. 
 */



#include <sys/socket.h>
#include <netpacket/packet.h>
#include <net/ethernet.h>
#include <arpa/inet.h>
#include <strings.h>
#include <net/if.h>
#include <string.h>
#include <sys/ioctl.h>
#include <unistd.h>
#include <errno.h>
#include "erl_nif.h"
#include "erl_driver.h"

static ERL_NIF_TERM read_clt(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM write_clt(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM read_srv(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM write_srv(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM read_socket(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[], int socket_fd);
static ERL_NIF_TERM write_socket(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[], int socket_fd);
static int          init_socket();
static int          clean_socket();
static int          bind_socket(int sd, char *nic);

#define MTU 1500

static int   cltside_socket;
static char *cltside_nic = "p2p1";
static int   srvside_socket;
static char *srvside_nic = "p7p1";

static ErlNifFunc nif_funcs[] =
{
        {"read_clt", 0, read_clt},
        {"write_clt", 1, write_clt},
        {"read_srv", 0, read_srv},
        {"write_srv", 1, write_srv},
};

static int load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
        if (init_socket() != 0) {
                return -1;
        }

        return 0;
}

static int upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data, 
                   ERL_NIF_TERM load_info)
{
        if (clean_socket() != 0) {
                return -1;
        }
        if (init_socket() != 0) {
                return -1;
        }

        return 0;
}

static void unload(ErlNifEnv *env, void *priv_data)
{
        clean_socket();

        return;
}

ERL_NIF_INIT(packet, nif_funcs, &load, NULL, &upgrade, &unload);

static ERL_NIF_TERM read_clt(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
        return read_socket(env, argc, argv, cltside_socket);
}

static ERL_NIF_TERM write_clt(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
        return write_socket(env, argc, argv, cltside_socket);
}

static ERL_NIF_TERM read_srv(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
        return read_socket(env, argc, argv, srvside_socket);
}

static ERL_NIF_TERM write_srv(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
        return write_socket(env, argc, argv, srvside_socket);
}

static int init_socket()
{

        /* client side nic */
        cltside_socket = socket(AF_PACKET, SOCK_RAW, htons(ETH_P_ALL));
        if (cltside_socket == -1) {
                return -1;
        }
        if (bind_socket(cltside_socket, cltside_nic) == -1) {
                return -1;
        }

        /* server side nic */
        srvside_socket = socket(AF_PACKET, SOCK_RAW, htons(ETH_P_ALL));
        if (srvside_socket == -1) {
                return -1;
        }
        if (bind_socket(srvside_socket, srvside_nic) == -1) {
                return -1;
        }

        return 0;
}

static int bind_socket(int sd, char *nic)
{
        struct ifreq ifr;

        bzero(&ifr, sizeof(ifr));
        strncpy((char *)ifr.ifr_name, nic, IFNAMSIZ); 
        if ((ioctl(sd, SIOCGIFINDEX , &ifr)) < 0) { 
                return -1;
        }

        if (setsockopt(sd, SOL_SOCKET, SO_BINDTODEVICE, &ifr, sizeof(ifr)) < 0) {
                return -1;
        }
                
        return 0;
}

static int clean_socket()
{
        close(cltside_socket);
        close(srvside_socket);

        return 0;
}

static ERL_NIF_TERM read_socket(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[], int socket_fd)
{
        ErlNifBinary buf = {0};
        ssize_t      ret_size;
        int          err;

        if (!enif_alloc_binary(MTU, &buf)) {
                return enif_make_tuple2(env, enif_make_atom(env, "error"), 
                                        enif_make_atom(env, "mem_error"));
        }
        if ((ret_size = read(socket_fd, buf.data, buf.size)) == -1) {
                err = errno;
                enif_release_binary(&buf);
                return enif_make_tuple2(env, enif_make_atom(env, "error"), 
                                        enif_make_atom(env, erl_errno_id(err)));
        }

        return enif_make_tuple2(env, enif_make_atom(env, "ok"), 
                                enif_make_binary(env, &buf));
}

static ERL_NIF_TERM write_socket(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[], int socket_fd)
{
        ErlNifBinary buf = {0};

        if (!enif_inspect_binary(env, argv[0], &buf)) {
                return enif_make_badarg(env);
        }

        if (write(socket_fd, buf.data, buf.size) == -1) {
                return enif_make_tuple2(env, enif_make_atom(env, "error"), 
                                        enif_make_atom(env, erl_errno_id(errno)));
                
        }

        return enif_make_atom(env, "ok");
}
