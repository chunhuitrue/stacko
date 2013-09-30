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
#include <fcntl.h>
#include <errno.h>
#include "erl_nif.h"
#include "erl_driver.h"


#define NICNAMELEN 64
#define MAXNIC     8
#define MAXFRAM    1518


static int          init();
static int          clean();
static ERL_NIF_TERM nic_up(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nic_down(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nic_recv(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nic_send(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static int          clean_nic_info(int i);


typedef struct nic {
        char nic_name[NICNAMELEN];
        int  hw_type;
        int  mtu;
        struct ifreq ifr
} nic_info;


static nic_info nic[MAXNIC];
static int      sockfd;


static ErlNifFunc nif_funcs[] =
{
        {"nic_up", 1, nic_up},
        {"nic_down", 1, nic_down},
        {"nic_recv", 0, nic_recv},
        {"nic_send", 2, nic_send}
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


ERL_NIF_INIT(nif, nif_funcs, &load, NULL, &upgrade, &unload);


static int init()
{
        int i;

        for (i = 0; i < MAXNIC; i++) {
                clean_nic_info(i);
        }

        sockfd = -1;

        return 0;
}


static int clean()
{
        return 0;
}


static ERL_NIF_TERM nic_up(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
        int  i;
        int  sd;
        int  val;
        int  index = -1;
        char buf[6];
        char name[NICNAMELEN];

        if (sockfd == -1) {
                sockfd = socket(AF_PACKET, SOCK_RAW, htons(ETH_P_ALL));
                val    = fcntl(sockfd, F_GETFL, 0);
                fcntl(sockfd, F_SETFL, val | O_NONBLOCK);
        }
        if (sockfd < 0) {
                return enif_make_tuple2(env, 
                                        enif_make_atom(env, "error"), 
                                        enif_make_atom(env, "socket"));
        }

        bzero(name, sizeof(name));
        enif_get_atom(env, argv[0], name, sizeof(name), ERL_NIF_LATIN1);

        for (i = 0; i < MAXNIC; i++) {
                if (strcmp(name, nic[i].nic_name) == 0) {
                        return enif_make_tuple2(env, 
                                                enif_make_atom(env, "error"), 
                                                enif_make_atom(env, "replicate"));
                }
        }

        for (i = 0; i < MAXNIC; i++) {
                if (strlen(nic[].nic_name) == 0 ) {
                        index = i;
                        break;
                }
        }
        if (index == -1) {
                return enif_make_tuple2(env, 
                                        enif_make_atom(env, "error"), 
                                        enif_make_atom(env, "too_many"));
        }

        strncpy(nic[index].nic_name, name, sizeof(nic[index].nic_name));
        if ((sd = socket(AF_PACKET, SOCK_RAW, htons(ETH_P_ALL))) < 0) {
                return enif_make_tuple2(env, 
                                        enif_make_atom(env, "error"), 
                                        enif_make_atom(env, "interface"));
        }
        bzero(&(nic[index].ifr), sizeof(nic[index].ifr));
        snprintf(nic[index].ifr.ifr_name, sizeof(nic[index].ifr), "%s", name);
        if (ioctl(sd, SIOCGIFHWADDR, &ifr) < 0) {
                return enif_make_tuple2(env, 
                                        enif_make_atom(env, "error"), 
                                        enif_make_atom(env, "hwaddr"));
        }
        nic[index].mtu     = 1500; /* todo */
        nic[index].hw_type = 1; /* todo */
        close(sd);
        
        memcpy(buf, nic[index].ifr.ifr_hwaddr.sa_data, 6);
        return enif_make_tuple4(enf,
                                enif_make_int(env, index),
                                enif_make_binary(env, &buf),
                                enif_make_int(nic[index].hw_type),
                                enif_make_int(nic[index].mtu));
}


static ERL_NIF_TERM nic_down(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
        int  i;
        int  index = -1;
        char name[NICNAMELEN];

        bzero(name, sizeof(name));
        enif_get_atom(env, argv[0], name, sizeof(name), ERL_NIF_LATIN1);

        for (i = 0; i < MAXNIC; i++) {
                if (strcmp(name, nic[i].nic_name) == 0) {
                        index = i;
                        clean_nic_info(i);
                        break;
                }
        }
        if (index == -1) {
                return enif_make_tuple2(env, 
                                        enif_make_atom(env, "error"), 
                                        enif_make_atom(env, "no_name"));
        }

        for (i = 0; i < MAXNIC; i++) {
                if (strcmp(name, nic[i].nic_name) != 0) {
                        return enif_make_atom(env, "ok");
                        break;
                }
        }

        close(sockfd);
        return enif_make_atom(env, "ok");
}


static ERL_NIF_TERM nic_recv(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
        int          err;
        ssize_t      ret_size;
        ErlNifBinary buf   = {0};

        if (!enif_alloc_binary(MAXFRAM, &buf)) {
                return enif_make_tuple2(env, 
                                        enif_make_atom(env, "error"), 
                                        enif_make_atom(env, "alloc"));
        }

        if ((ret_size = read(sockfd, buf.data, buf.size)) == -1) {
                err = errno;
                enif_release_binary(&buf);
                return enif_make_tuple2(env, 
                                        enif_make_atom(env, "error"), 
                                        enif_make_atom(env, erl_errno_id(err)));
        }
        
        return enif_make_tuple2(env,
                                enif_make_binary(env, &buf));
}


static ERL_NIF_TERM nic_send(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
        int          index = -1;
        ErlNifBinary buf   = {0};

        enif_get_int(env, argv[0], &index);
        if (index < 0) {
                return enif_make_tuple2(env, 
                                        enif_make_atom(env, "error"), 
                                        enif_make_atom(env, "arg"));
        }

        if (!enif_inspect_binary(env, argv[1], &buf)) {
                return enif_make_badarg(env);
        }

        if (write(nic[index].sockfd, buf.data, buf.size) == -1) {
                return enif_make_tuple2(env, enif_make_atom(env, "error"), 
                                        enif_make_atom(env, erl_errno_id(errno)));
        }

        return enif_make_atom(env, "ok");
}


static ERL_NIF_TERM write_nic(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
        int          index = -1;
        ErlNifBinary buf   = {0};

        enif_get_int(env, argv[0], &index);
        if (index < 0) {
                return enif_make_tuple2(env, 
                                        enif_make_atom(env, "error"), 
                                        enif_make_atom(env, "arg"));
        }

        if (!enif_inspect_binary(env, argv[1], &buf)) {
                return enif_make_badarg(env);
        }

        if (write(nic[index].sockfd, buf.data, buf.size) == -1) {
                return enif_make_tuple2(env, enif_make_atom(env, "error"), 
                                        enif_make_atom(env, erl_errno_id(errno)));
        }

        return enif_make_atom(env, "ok");
}


static int clean_nic_info(int i)
{
        bzero(nic[i].nic_name, sizeof(nic[i].nic_name));
        bzero(&(nic[i].ifr), sizeof(nic[i].ifr));

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

