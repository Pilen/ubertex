
/* #include <setjmp.h> */
/* #include <mupdf/fitz.h> */

#include "types.h"
#include "pdf.h"
#include "assert.h"
#include "string.h"

void *pdf_load(Value skeleton, Unt initial_score) {
    /* z_assert(skeleton.type == PDF); */
    /* Pdf *pdf = skeleton.val.pdf_val; */
    /* fz_context *context = fz_new_context(NULL, NULL, FZ_STORE_UNLIMITED); */
    /* z_assert(context); */
    /* fz_register_document_handlers(context); */

    /* fz_document *document = fz_open_document(context, pdf -> path.val.string_val -> text); */
    /* if (!document) { */
    /*     return NULL; */
    /* } */
    /* pdf -> pagecount = fz_count_pages(context, document); */

    /* /\* for (Int i = 0; i < pdf -> pagecount; i++) { *\/ */
    /* /\*     fz_page *page = fz_load_page(document, i); *\/ */
    /* /\* } *\/ */
    /* fz_page *page = fz_load_page(context, document, 1); */
    /* assert(page); */
    /* fz_matrix transform; */
    /* float rotation = 0; */
    /* fz_rotate(&transform, rotation); */
    /* float zoom = 100; */
    /* fz_pre_scale(&transform, zoom / 100.0f, zoom / 100.0f); */

    /* fz_rect bounds; */
    /* fz_bound_page(context, page, &bounds); */
    /* fz_transform_rect(&bounds, &transform); */

    /* fz_irect bbox; */
    /* fz_round_rect(&bbox, &bounds); */
    /* fz_colorspace *colorspace = fz_device_rgb(context); */
    /* fz_pixmap *pixmap = fz_new_pixmap_with_bbox(context, colorspace, &bbox); */
    /* assert(pixmap); */
    /* fz_clear_pixmap_with_value(context, pixmap, 0xff); */

    /* fz_device *device = fz_new_draw_device(context, pixmap); */
    /* fz_run_page(context, page, device, &transform, NULL); */
    /* fz_drop_device(context, device); */

    /* fz_write_png(context, pixmap, "/tmp/out.png", 0); */
    /* fz_drop_pixmap(context, pixmap); */
    /* fz_drop_page(context, page); */
    /* fz_drop_document(context, document); */
    /* fz_drop_context(context); */

    /* pdf -> refcount = 0; */
    /* pdf -> score = initial_score; */
    /* pdf -> context = context; */

    return NULL;
}
