--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

private with Interfaces.C;

package Blake_3 is

   subtype blake3_hash is String (1 .. 32);
   subtype blake3_hash_hex is String (1 .. 64);

   function file_digest (path : String; power : Positive := 16) return blake3_hash;
   function digest (input_string : String) return blake3_hash;
   function hex (hash : blake3_hash) return blake3_hash_hex;

   function b3_hashsize return Natural;

private

   package IC renames Interfaces.C;
   use type IC.size_t;

   BLAKE3_BLOCK_LEN : constant IC.size_t := 64;
   BLAKE3_MAX_DEPTH : constant IC.size_t := 54;
   BLAKE3_OUT_LEN   : constant IC.size_t := 32;   -- must match blake3_hash length

   type uint32 is mod 2 ** 32;
   type uint64 is mod 2 ** 64;
   type b3_cv  is array (1 .. 8) of uint32;
   type b3_key is array (1 .. 8) of uint32;
   type b3_buf is array (1 .. BLAKE3_BLOCK_LEN) of IC.unsigned_char;
   type b3_cvs is array (1 .. (BLAKE3_MAX_DEPTH + 1) * BLAKE3_OUT_LEN) of IC.unsigned_char;
   subtype hexrep is String (1 .. 2);

   type blake3_chunk_state is
      record
         cv                : b3_cv;
         chunk_counter     : uint64;
         buf               : b3_buf;
         buf_len           : IC.unsigned_char;
         blocks_compressed : IC.unsigned_char;
         flags             : IC.unsigned_char;
      end record;

   type blake3_hasher is
      record
         key          : b3_key;
         chunk        : blake3_chunk_state;
         cv_stack_len : IC.unsigned_char;
         cv_stack     : b3_cvs;
      end record;

   type blake3_hasher_Access is access all blake3_hasher;
   pragma Convention (C, blake3_hasher_Access);

   procedure b3_init (self : blake3_hasher_Access);
   pragma Import (C, b3_init, "blake3_hasher_init");

   procedure b3_update  (self : blake3_hasher_Access; plain : String);
   function b3_finalize (self : blake3_hasher_Access) return blake3_hash;

   procedure C_b3hasher_update
     (self : blake3_hasher_Access;
      data : access IC.unsigned_char;
      len  : IC.size_t);
   pragma Import (C, C_b3hasher_update, "blake3_hasher_update");

   procedure C_b3hasher_finalize
     (self : blake3_hasher_Access;
      hash : access IC.unsigned_char;
      len  : IC.size_t);
   pragma Import (C, C_b3hasher_finalize, "blake3_hasher_finalize");

   function char2hex (quattro : Character) return hexrep;

end Blake_3;
