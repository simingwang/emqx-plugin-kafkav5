/**
 * Copyright 2011,  Filipe David Manana  <fdmanana@apache.org>
 * Web:  http://github.com/fdmanana/snappy-erlang-nif
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 **/

#include <iostream>
#include <cstring>

#include "erl_nif_compat.h"
#include "snappy.h"
#include "snappy-sinksource.h"

#ifdef __cplusplus
#define BEGIN_C extern "C" {
#define END_C }
#else
#define BEGIN_C
#define END_C
#endif

#define SC_PTR(c) reinterpret_cast<char *>(c)

class SnappyNifSink : public snappy::Sink
{
    public:
        SnappyNifSink(ErlNifEnv* e);
        ~SnappyNifSink();

        void Append(const char* data, size_t n);
        char* GetAppendBuffer(size_t len, char* scratch);
        ErlNifBinary& getBin();

    private:
        ErlNifEnv* env;
        ErlNifBinary bin;
        size_t length;
};

SnappyNifSink::SnappyNifSink(ErlNifEnv* e) : env(e), length(0)
{
    if(!enif_alloc_binary_compat(env, 0, &bin)) {
        env = NULL;
        throw std::bad_alloc();
    }
}

SnappyNifSink::~SnappyNifSink()
{
    if(env != NULL) {
        enif_release_binary_compat(env, &bin);
    }
}

void
SnappyNifSink::Append(const char *data, size_t n)
{
    if(data != (SC_PTR(bin.data) + length)) {
        memcpy(bin.data + length, data, n);
    }
    length += n;
}

char*
SnappyNifSink::GetAppendBuffer(size_t len, char* scratch)
{
    size_t sz;

    if((length + len) > bin.size) {
        sz = (len * 4) < 8192 ? 8192 : (len * 4);

        if(!enif_realloc_binary_compat(env, &bin, bin.size + sz)) {
            throw std::bad_alloc();
        }
    }

    return SC_PTR(bin.data) + length;
}

ErlNifBinary&
SnappyNifSink::getBin()
{
    if(bin.size > length) {
        if(!enif_realloc_binary_compat(env, &bin, length)) {
            throw std::bad_alloc();
        }
    }
    return bin;
}


static inline ERL_NIF_TERM
make_atom(ErlNifEnv* env, const char* name)
{
    ERL_NIF_TERM ret;
    if(enif_make_existing_atom_compat(env, name, &ret, ERL_NIF_LATIN1)) {
        return ret;
    }
    return enif_make_atom(env, name);
}


static inline ERL_NIF_TERM
make_ok(ErlNifEnv* env, ERL_NIF_TERM mesg)
{
    ERL_NIF_TERM ok = make_atom(env, "ok");
    return enif_make_tuple2(env, ok, mesg);
}


static inline ERL_NIF_TERM
make_error(ErlNifEnv* env, const char* mesg)
{
    ERL_NIF_TERM error = make_atom(env, "error");
    return enif_make_tuple2(env, error, make_atom(env, mesg));
}


BEGIN_C


ERL_NIF_TERM
snappy_compress_erl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary input;

    if(!enif_inspect_iolist_as_binary(env, argv[0], &input)) {
        return enif_make_badarg(env);
    }

    // If empty binary has been provided, return an empty binary.
    // Snappy will do this in any case, so might as well skip the
    // overhead...
    if(input.size == 0) {
        ErlNifBinary empty;
        // init empty;
        memset(&empty,0,sizeof(ErlNifBinary));
        return make_ok(env, enif_make_binary(env,&empty));
     }

    try {
        snappy::ByteArraySource source(SC_PTR(input.data), input.size);
        SnappyNifSink sink(env);
        snappy::Compress(&source, &sink);
        return make_ok(env, enif_make_binary(env, &sink.getBin()));
    } catch(std::bad_alloc e) {
        return make_error(env, "insufficient_memory");
    } catch(...) {
        return make_error(env, "unknown");
    }
}


ERL_NIF_TERM
snappy_decompress_erl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary bin;
    ErlNifBinary ret;
    size_t len;

    if(!enif_inspect_iolist_as_binary(env, argv[0], &bin)) {
        return enif_make_badarg(env);
    }

    // Check that the binary is not empty
    if(bin.size == 0) {
        // Snappy library cannot decompress an empty binary - although
        // it will unfortunately let you compress one. If an empty binary
        // has been passed - send an empty binary back.
        memset(&ret,0,sizeof(ErlNifBinary));
        return make_ok(env, enif_make_binary(env,&ret));
    }

    try {
        if(!snappy::GetUncompressedLength(SC_PTR(bin.data), bin.size, &len)) {
            return make_error(env, "data_not_compressed");
        }

        if(!enif_alloc_binary_compat(env, len, &ret)) {
            return make_error(env, "insufficient_memory");
        }

        if(!snappy::RawUncompress(SC_PTR(bin.data), bin.size,
                                            SC_PTR(ret.data))) {
            return make_error(env, "corrupted_data");
        }

        return make_ok(env, enif_make_binary(env, &ret));
    } catch(...) {
        return make_error(env, "unknown");
    }
}


ERL_NIF_TERM
snappy_uncompressed_length_erl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary bin;
    size_t len;

    if(!enif_inspect_iolist_as_binary(env, argv[0], &bin)) {
        return enif_make_badarg(env);
    }

    try {
        if(!snappy::GetUncompressedLength(SC_PTR(bin.data), bin.size, &len)) {
            return make_error(env, "data_not_compressed");
        }
        return make_ok(env, enif_make_ulong(env, len));
    } catch(...) {
        return make_error(env, "unknown");
    }
}


ERL_NIF_TERM
snappy_is_valid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary bin;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &bin)) {
        return enif_make_badarg(env);
    }

    try {
        if(snappy::IsValidCompressedBuffer(SC_PTR(bin.data), bin.size)) {
            return make_atom(env, "true");
        } else {
            return make_atom(env, "false");
        }
    } catch(...) {
        return make_error(env, "unknown");
    }
}


static int
on_load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    return 0;
}


static int
on_reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    return 0;
}


static int
on_upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM info)
{
    return 0;
}


static ErlNifFunc nif_functions[] = {
    {"compress", 1, snappy_compress_erl},
    {"decompress", 1, snappy_decompress_erl},
    {"uncompressed_length", 1, snappy_uncompressed_length_erl},
    {"is_valid", 1, snappy_is_valid}
};


ERL_NIF_INIT(snappyer, nif_functions, &on_load, &on_reload, &on_upgrade, NULL);


END_C
