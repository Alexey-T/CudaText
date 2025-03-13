        s = '''
        file lines count: {cnt}
        '''.format(
                cnt = ed.get_line_count()
            )
        msg_box(s, MB_OK)

