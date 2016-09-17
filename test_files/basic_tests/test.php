<?php

if (!isset($db)) {

    class my_DB extends DB {

        function connect($phptype = 'mysql') {
            $dsn = "mysql://" . DB_USER . ":" . DB_PASSWORD . "@" . DB_SERVER . "/" . DB_DATABASE;
            $op = array(
                'debug' => 2,
                'portability' => DB_PORTABILITY_ALL,
            );

            //PEAR::setErrorHandling(PEAR_ERROR_TRIGGER);

            $db = & parent::connect($dsn, $op);

            if (parent::isError($db)) {
                die($db->getMessage());
            }
            return $db;
        }

    }

    $db = &my_DB::connect();
    $db->query("SET NAMES cp1251");
}

$array = explode(",", $part_id);
