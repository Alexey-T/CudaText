from .css_data import *

class Command:
    def get_props(self):
        return list(properties_dict.keys())
        
    def get_prop_vals(self, name):
        r = []
        values = properties_dict.get(name, [])
        values += allowed_values
        for val in values:
            if type(val) is str:
                if val.startswith('<') and val.endswith('>'):
                    more = common_values.get(val[1:-1], [])
                    for m in more:
                        if type(m) is str:
                            r.append(m)
                        elif type(m) is list:
                            r.append(m[0])
                else:
                    r.append(val)
            elif type(val) is list:
                r.append(val[0])
        return r
