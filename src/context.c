#include "opium/opium.h"

#include <string.h>
#include <dlfcn.h>

void
opi_context_init(struct opi_context *ctx)
{
  opi_ptrvec_init(&ctx->types);
  opi_ptrvec_init(&ctx->bc);
  opi_strvec_init(&ctx->dl_paths);
  opi_ptrvec_init(&ctx->dls);
}

static void
delete_dl(void *dl)
{ dlclose(dl); }

void
opi_context_destroy(struct opi_context *ctx)
{
  opi_ptrvec_destroy(&ctx->types, (void*)opi_type_delete);
  opi_ptrvec_destroy(&ctx->bc, (void*)opi_insn_delete);
  opi_strvec_destroy(&ctx->dl_paths);
  opi_ptrvec_destroy(&ctx->dls, delete_dl);
}

void
opi_context_add_type(struct opi_context *ctx, opi_type_t type)
{ opi_ptrvec_push(&ctx->types, type, NULL); }

void
opi_context_drain_bytecode(struct opi_context *ctx, struct opi_bytecode *bc)
{ opi_ptrvec_push(&ctx->bc, opi_bytecode_drain(bc), NULL); }

void
opi_context_add_dl(struct opi_context *ctx, const char *path, void *dl)
{
  opi_strvec_push(&ctx->dl_paths, path);
  opi_ptrvec_push(&ctx->dls, dl, NULL);
}

void*
opi_context_find_dl(struct opi_context *ctx, const char *path)
{
  int idx = opi_strvec_find(&ctx->dl_paths, path);
  return idx < 0 ? NULL : ctx->dls.data[idx];
}

int
opi_is_dl(const char *path)
{
  static char elf_header[] = { 0x7F, 'E', 'L', 'F' };
  char header[4];

  FILE *fs = fopen(path, "r");
  opi_assert(fs);
  opi_assert(fread(header, 1, 4, fs) == 4);
  opi_assert(fclose(fs) == 0);

  return memcmp(header, elf_header, sizeof(elf_header)) == 0;
}
