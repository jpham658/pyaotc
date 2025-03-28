#include "string_utils.h"

bool str_eq(const char *str1, const char *str2)
{
    return strcmp(str1, str2) == 0;
}

size_t str_len(const char *str)
{
    return strlen(str);
}

char *str_concat(const char *str1, const char *str2)
{
    size_t new_str_len = strlen(str1) + strlen(str2) + 1;
    char *new_str_ptr = (char *)malloc(new_str_len);
    strcpy(new_str_ptr, str1);
    strcat(new_str_ptr, str2);
    return new_str_ptr;
}

char *str_mult(const char *str, word n)
{
    if (n <= 0)
    {
        return "";
    }

    int len = strlen(str);
    char *result = (char *)malloc(len * n + 1);
    if (!result)
    {
        return NULL;
    }

    result[0] = '\0';
    for (int i = 0; i < n; i++)
    {
        strcat(result, str);
    }

    return result;
}

const char **str_index(const char *str, word index)
{
    size_t len = str_len(str);

    if (index < 0)
    {
        index += len;
    }
    if (index < 0 || index >= len)
    {
        fprintf(stderr, "Index out of bounds: %ld (length: %zu)\n", index, len);
        exit(EXIT_FAILURE);
    }

    char *res = (char *)GC_malloc(2 * sizeof(char));
    res[0] = str[index];
    res[1] = '\0';
    const char **res_ptr = (const char **)GC_malloc(sizeof(const char *));
    *res_ptr = res;
    return res_ptr;

    return res_ptr;
}

bool str_is_truthy(const char *str)
{
    return !(strcmp(str, "") == 0);
}

void *str_next(void *iterator)
{
    Iterator *iter = (Iterator *)iterator;
    const char *str = (const char *)iter->data;

    if (iter->current < iter->length)
    {
        char *res = (char *)GC_malloc(2 * sizeof(char));
        res[0] = str[iter->current];
        res[1] = '\0';
        iter->current++;

        const char **res_ptr = (const char **)GC_malloc(sizeof(const char *));
        *res_ptr = res;
        return res_ptr;
    }
    return NULL;
}

Iterator *str_iter(const char *str)
{
    size_t length = strlen(str);
    const char *str_copy = (const char *)GC_strdup(str);
    return create_iterator(str, sizeof(char), length, str_next, StringIter);
}