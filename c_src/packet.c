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


#define NICNAMELEN 64
#define MAXNIC     8
#define MTU        1500


static int          init();
static int          clean();
static ERL_NIF_TERM open_nic(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM close_nic(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM read_nic(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM write_nic(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static int          clean_nic_info(int i);
static int          bind_socket(int sd, char *nic);


typedef struct nic {
        char nic_name[NICNAMELEN];
        int  socket_fd;
} nic_info;


static nic_info nic[MAXNIC];


static ErlNifFunc nif_funcs[] =
{
        {"open_nic", 1, open_nic},
        {"close_nic", 1, close_nic},
        {"read_nic", 1, read_nic},
        {"write_nic", 2, write_nic}
};


static int load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
        if (init() != 0) {
                return -1;
        }

        return 0;
}


static int upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data, 
                   ERL_NIF_TERM load_info)
{
        if (clean() != 0) {
                return -1;
        }
        if (init() != 0) {
                return -1;
        }

        return 0;
}


static void unload(ErlNifEnv *env, void *priv_data)
{
        clean();

        return;
}


ERL_NIF_INIT(packet, nif_funcs, &load, NULL, &upgrade, &unload);


static int init()
{
        int i;

        for (i = 0; i < MAXNIC; i++) {
                clean_nic_info(i);
        }

        return 0;
}


static int clean()
{
        return 0;
}


static ERL_NIF_TERM open_nic(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
        int  i;
        int  index = -1;
        char name[NICNAMELEN];

        bzero(name, sizeof(name));
        enif_get_atom(env, argv[0], name, sizeof(name), ERL_NIF_LATIN1);
        for (i = 0; i < MAXNIC; i++) {
                if (strcmp(name, nic[i].nic_name) == 0) {
                        return enif_make_tuple2(env, enif_make_atom(env, "error"), 
                                                enif_make_atom(env, "replicate"));
                }
        }

        for (i = 0; i < MAXNIC; i++) {
                if (nic[i].socket_fd != -1) {
                        index = i;
                        break;
                }
        }
        if (index == -1) {
                return enif_make_tuple2(env, enif_make_atom(env, "error"), 
                                        enif_make_atom(env, "too_many"));
        }

        nic[index].socket_fd = socket(AF_PACKET, SOCK_RAW, htons(ETH_P_ALL));
        if (nic[index].socket_fd == -1) {
                return enif_make_tuple2(env, enif_make_atom(env, "error"), 
                                        enif_make_atom(env, "socket"));
        }
        if ((bind_socket(nic[index].socket_fd, nic[index].nic_name)) == -1) {
                return enif_make_tuple2(env, enif_make_atom(env, "error"), 
                                        enif_make_atom(env, "bind"));
        }

        return enif_make_int(env, index);
}


static ERL_NIF_TERM close_nic(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
        int  i;
        char name[NICNAMELEN];

        bzero(name, sizeof(name));
        enif_get_atom(env, argv[0], name, sizeof(name), ERL_NIF_LATIN1);
        for (i = 0; i < MAXNIC; i++) {
                if (strcmp(name, nic[i].nic_name) == 0) {
                        close(nic[i].socket_fd);
                        clean_nic_info(i);
                        return enif_make_atom(env, "ok");
                }
        }

        return enif_make_tuple2(env, enif_make_atom(env, "error"), 
                                enif_make_atom(env, "no_name"));
}


static ERL_NIF_TERM read_nic(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
        return enif_make_atom(env, "ok");
}


static ERL_NIF_TERM write_nic(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
        return enif_make_atom(env, "ok");
}


static int clean_nic_info(int i)
{
        bzero(nic[i].nic_name, sizeof(nic[i].nic_name));
        nic[i].socket_fd = -1;

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

