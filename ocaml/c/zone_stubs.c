#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>

/* This stuff is courtesy the UPPAAL DBM library. */
#ifdef __cplusplus

#endif
#include <dbm/constraints.h>
#include <dbm/dbm.h>

/* Encapsulation of opaque window handles (of type RAW_T *)
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

/* Encapsulation of opaque window handles (of type CONSTRAINT_T *)
   as Caml custom blocks. */
static struct custom_operations udbm_constraint_t_ops = {
  "fr.inria.caml.udbm_constraint_t",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

/* Accessing the CONSTRAINT_T * part of a Caml custom block */
#define constraint_t_val(v) (*((constraint_t **) Data_custom_val(v)))

/* Allocating a Caml custom block to hold the given CONSTRAINT_T * */
static value alloc_constraint_t(constraint_t * w)
{
  value v = alloc_custom(&udbm_constraint_t_ops, sizeof(constraint_t *), 0, 1);
  constraint_t_val(v) = w;
  return v;
}

CAMLprim value zone_dbm_init (value dim) {
  CAMLparam1(dim);
  raw_t *dbm = (raw_t *)malloc(dim*dim*sizeof(raw_t));
  dbm_init(dbm, Int_val(dim));
  CAMLreturn (alloc_raw_t(dbm));
}

/*No idea how this will ever be used.*/
CAMLprim value zone_dbm_finish (value dbm) {
  CAMLparam1(dbm);
  free((raw_t *)raw_t_val(dbm));
  CAMLreturn (Val_unit);
}

CAMLprim value zone_dbm_constrainC (value dbm, value dim, value c) {
  CAMLparam3(dbm, dim, c);
  /* printf("(constraint_t_val(c))->i = %d\n", (constraint_t_val(c))->i); */
  /* printf("(constraint_t_val(c))->j = %d\n", (constraint_t_val(c))->j); */
  dbm_constrainC(raw_t_val(dbm), Int_val(dim), *(constraint_t_val(c)));
  CAMLreturn (dbm);
}

CAMLprim value zone_dbm_constraint2 (value i,
				     value j,
				     value bound,
				     value isStrict) {
  CAMLparam4(i, j, bound, isStrict);
  constraint_t *constraint = (constraint_t *)malloc(sizeof(constraint_t *));
  *constraint = dbm_constraint2((cindex_t)Int_val(i),
		  (cindex_t)Int_val(j),
		  (int32_t)Int_val(bound),
		  isStrict == Val_true ? TRUE : FALSE);
  CAMLreturn (alloc_constraint_t(constraint));
}

CAMLprim value zone_dbm_isEmpty (value dbm, value dim) {
  CAMLparam2(dbm, dim);
  if (dbm_isEmpty(raw_t_val(dbm), Int_val(dim)) == TRUE) {
    CAMLreturn (Val_int(1)); //ocaml true
  } else {
    CAMLreturn (Val_int(0)); //ocaml false
  }
}

CAMLprim value zone_dbm_haveIntersection (value dst, value src, value dim) {
  CAMLparam3(dst, src, dim);
  if (dbm_intersection(raw_t_val(dst), raw_t_val(src), Int_val(dim)) == TRUE) {
    CAMLreturn (Val_int(1)); //ocaml true
  } else {
    CAMLreturn (Val_int(0)); //ocaml false
  }
}

CAMLprim value zone_dbm_intersection (value dst, value src, value dim) {
  CAMLparam3(dst, src, dim);
  dbm_intersection(raw_t_val(dst), raw_t_val(src), Int_val(dim));
  CAMLreturn (dst);
}

CAMLprim value zone_dbm_freeClock (value dbm, value dim, value k) {
  CAMLparam3(dbm, dim, k);
  dbm_freeClock(raw_t_val(dbm), Int_val(dim), Int_val(k));
  CAMLreturn (dbm);
}

CAMLprim value zone_dbm_updateValue (value dbm, value dim, value k, value val) {
  CAMLparam4(dbm, dim, k, val);
  dbm_updateValue(raw_t_val(dbm), Int_val(dim), Int_val(k), Int_val(val));
  CAMLreturn (dbm);
}
