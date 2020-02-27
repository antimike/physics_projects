import pyexcel as data_reader

def filter_dict_by_key_list(dict, *key_list):
    return dict if (not bool(key_list) or key_list is None or not any(key_list)) \
        else {k: v for k, v in dict.items() if k in key_list}

def transform_dict(dict, fn):
    return {k: fn(k, v) for k, v in dict.items()}

def get_columns_from_sheet(sheet, *column_names):
    del sheet.row[filter_row]
    sheet.name_columns_by_row(0)
    return filter_dict_by_key_list(sheet.to_dict(), *column_names)

def get_sheets_from_workbook(filename, *sheet_names):
    book = data_reader.get_book(file_name = filename)
    dict = filter_dict_by_key_list({sheet.name: sheet for sheet in book}, *sheet_names)
    for sheet in dict.values():
        del sheet.row[filter_row]
    return dict

def get_data_from_workbook(filename, target_struct):
    target_struct = target_struct or {}
    def get_cols_for_sheet(name, sheet):
        return get_columns_from_sheet(sheet, *(target_struct.get(name, [])))
    return transform_dict(
        get_sheets_from_workbook(filename, *target_struct.keys()),
        get_cols_for_sheet
    )

def filter_row(row_index, row):
    result = [element for element in row if element != '']
    return len(result)==0

def clean_sheet(sheet):
    del sheet.row[filter_row]
