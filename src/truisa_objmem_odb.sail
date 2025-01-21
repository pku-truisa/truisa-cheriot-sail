/*=======================================================================================*/
/*  This Sail RISC-V architecture model, comprising all files and                        */
/*  directories except where otherwise noted is subject the BSD                          */
/*  two-clause license in the LICENSE file.                                              */
/*                                                                                       */
/*  SPDX-License-Identifier: BSD-2-Clause                                                */
/*=======================================================================================*/

/* OTE attributes, permission checks and updates */

type oteAttribs = bits(2)

bitfield OTE_Bits : oteAttribs = {
  R : 1
  W : 0
}

/* idealized generic ODB entry to model fence.vm and also speed up simulation. */
struct ODB_Entry('asidlen: Int, 'valen: Int, 'otelen: Int) = {
  asid : bits('asidlen),
  vAddr : bits('valen),      /* Virtual Address */
  ote : bits('otelen),       /* OTE */
  age : bits(64)
}


val make_ODB_Entry : forall 'asidlen 'valen 'otelen, 'valen > 0.
  (bits('asidlen), bits('valen), bits('otelen))
  -> ODB_Entry('asidlen, 'valen, 'otelen)
function make_ODB_Entry(asid, vAddr, ote) = {
  struct {
    asid = asid,
    ote = ote,
    vAddr = vAddr,
    age = mcycle
  }
}

val match_ODB_Entry : forall 'asidlen 'valen 'otelen.
  (ODB_Entry('asidlen, 'valen, 'otelen), bits('asidlen), bits('valen))
  -> bool
function match_ODB_Entry(ent, asid, vaddr) =
  ( ent.asid == asid & ent.vAddr == vaddr)

val flush_ODB_Entry : forall 'asidlen 'valen 'otelen.
  (ODB_Entry('asidlen, 'valen, 'otelen), option(bits('asidlen)), option(bits('valen)))
  -> bool
function flush_ODB_Entry(e, asid, addr) = {
  match(asid, addr) {
    ( None(),  None()) => true,
    ( None(), Some(a)) => e.vAddr == a,
    (Some(i),  None()) => e.asid == i,
    (Some(i), Some(a)) => e.asid == i & e.vAddr == a
  }
}

/* ODB management: single entry for now */
type ote32   = bits(32)

// ideally we would use the below form:
// type ODB32_Entry = ODB_Entry(sizeof(asid32), sizeof(vaddr32), sizeof(ote32))
type ODB32_Entry = ODB_Entry(9, 32, 32)
register odb32 : option(ODB32_Entry)

val lookup_ODB32 : (asid32, vaddr32) -> option((nat, ODB32_Entry))
function lookup_ODB32(asid, vaddr) =
  match odb32 {
    None()  => None(),
    Some(e) => if match_ODB_Entry(e, asid, vaddr) then Some((0, e)) else None()
  }

val add_to_ODB32 : (asid32, vaddr32, ote32) -> unit
function add_to_ODB32(asid, vAddr, ote) = {
  let ent : ODB32_Entry = make_ODB_Entry(asid, Addr, ote.bits);
  odb32 = Some(ent)
}

function write_ODB32(idx : nat, ent : ODB32_Entry) -> unit =
  odb32 = Some(ent)

val flush_ODB32 : (option(asid32), option(vaddr32)) -> unit
function flush_ODB32(asid, addr) =
  match (odb32) {
    None()  => (),
    Some(e) => if   flush_ODB_Entry(e, asid, addr)
               then odb32 = None()
               else ()
  }

function init_objmem_rv32() -> unit = {
  odb32 = None()
}