//===-- PdbUtil.h -----------------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLDB_PLUGINS_SYMBOLFILENATIVEPDB_PDBUTIL_H
#define LLDB_PLUGINS_SYMBOLFILENATIVEPDB_PDBUTIL_H

#include "lldb/lldb-enumerations.h"

#include "llvm/DebugInfo/CodeView/SymbolRecord.h"
#include "llvm/DebugInfo/CodeView/TypeRecord.h"
#include "llvm/DebugInfo/PDB/PDBTypes.h"

#include <tuple>
#include <utility>

namespace lldb_private {
namespace npdb {

struct CVTagRecord {
  enum Kind { Class, Struct, Union, Enum };

  static CVTagRecord create(llvm::codeview::CVType type);

  Kind kind() const { return m_kind; }

  const llvm::codeview::TagRecord &asTag() const {
    if (m_kind == Struct || m_kind == Class)
      return cvclass;
    if (m_kind == Enum)
      return cvenum;
    return cvunion;
  }

  const llvm::codeview::ClassRecord &asClass() const {
    assert(m_kind == Struct || m_kind == Class);
    return cvclass;
  }

  const llvm::codeview::EnumRecord &asEnum() const {
    assert(m_kind == Enum);
    return cvenum;
  }

  const llvm::codeview::UnionRecord &asUnion() const {
    assert(m_kind == Union);
    return cvunion;
  }

private:
  CVTagRecord(llvm::codeview::ClassRecord &&c);
  CVTagRecord(llvm::codeview::UnionRecord &&u);
  CVTagRecord(llvm::codeview::EnumRecord &&e);
  union {
    llvm::codeview::ClassRecord cvclass;
    llvm::codeview::EnumRecord cvenum;
    llvm::codeview::UnionRecord cvunion;
  };
  Kind m_kind;
};

struct SegmentOffset {
  SegmentOffset() = default;
  SegmentOffset(uint16_t s, uint32_t o) : segment(s), offset(o) {}
  uint16_t segment = 0;
  uint32_t offset = 0;
};

struct SegmentOffsetLength {
  SegmentOffsetLength() = default;
  SegmentOffsetLength(uint16_t s, uint32_t o, uint32_t l)
      : so(s, o), length(l) {}
  SegmentOffset so;
  uint32_t length = 0;
};

llvm::pdb::PDB_SymType CVSymToPDBSym(llvm::codeview::SymbolKind kind);
llvm::pdb::PDB_SymType CVTypeToPDBType(llvm::codeview::TypeLeafKind kind);

bool SymbolHasAddress(const llvm::codeview::CVSymbol &sym);
bool SymbolIsCode(const llvm::codeview::CVSymbol &sym);

SegmentOffset GetSegmentAndOffset(const llvm::codeview::CVSymbol &sym);
SegmentOffsetLength
GetSegmentOffsetAndLength(const llvm::codeview::CVSymbol &sym);

template <typename RecordT> bool IsValidRecord(const RecordT &sym) {
  return true;
}

inline bool IsValidRecord(const llvm::codeview::ProcRefSym &sym) {
  // S_PROCREF symbols have 1-based module indices.
  return sym.Module > 0;
}

bool IsForwardRefUdt(llvm::codeview::CVType cvt);
bool IsTagRecord(llvm::codeview::CVType cvt);

lldb::AccessType TranslateMemberAccess(llvm::codeview::MemberAccess access);
llvm::codeview::TypeIndex GetFieldListIndex(llvm::codeview::CVType cvt);
llvm::codeview::TypeIndex
LookThroughModifierRecord(llvm::codeview::CVType modifier);

llvm::StringRef DropNameScope(llvm::StringRef name);

} // namespace npdb
} // namespace lldb_private

#endif
