#include "erl_nif.h"
#include "crc32c.h"

static ERL_NIF_TERM crc32c_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{

  uint32_t acc;
  if (!enif_get_uint(env, argv[0], &acc)) {
	  return enif_make_badarg(env);
  }

  ErlNifBinary bin;
  if(!enif_inspect_iolist_as_binary(env, argv[1], &bin)) {
    return enif_make_badarg(env);
  }

  uint32_t result = crc32c(acc, bin.data, bin.size);

	return enif_make_uint(env, result);
}

static int on_load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
  crc32c_global_init();
  return 0;
}

static ErlNifFunc nif_funs[] =
{
	    {"nif", 2, crc32c_nif}
};

ERL_NIF_INIT(crc32cer, nif_funs, on_load, NULL, NULL, NULL);

