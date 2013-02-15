#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>

/* This stuff is courtesy the UPPAAL DBM library. */
#include <dbm/constraints.h>
#include <dbm/dbm.h>

/* Encapsulation of opaque window handles (of type WINDOW *)
   as Caml custom blocks. */
static struct custom_operations udbm_raw_t_ops = {
  "fr.inria.caml.udbm_raw_t",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

/* Accessing the RAW_T * part of a Caml custom block */
#define raw_t_val(v) (*((raw_t **) Data_custom_val(v)))

/* Allocating a Caml custom block to hold the given RAW_T * */
static value alloc_raw_t(raw_t * w)
{
  value v = alloc_custom(&udbm_raw_t_ops, sizeof(raw_t *), 0, 1);
  raw_t_val(v) = w;
  return v;
}

CAMLprim value zone_dbm_init (value numclocks) {
  raw_t dbm;
  cindex_t dim = Int_val (numclocks) + 1;
  dbm_init(&dbm, dim);
  CAMLreturn (alloc_raw_t(dbm));
}
