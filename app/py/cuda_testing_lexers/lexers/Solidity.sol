// Sample Solidity for lexer benchmarking
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

import "./IERC20.sol";

contract BenchVault {
    uint256 public constant MAX_ITEMS = 557;
    string public constant PREFIX = "bench-";

    struct Entry {
        string name;
        uint256 weight;
        bool active;
    }

    mapping(address => Entry) private entries;
    address public owner;

    event EntryAdded(address indexed who, string name, uint256 weight);
    error NotOwner(address caller);

    constructor() {
        owner = msg.sender;
    }

    modifier onlyOwner() {
        if (msg.sender != owner) revert NotOwner(msg.sender);
        _;
    }

    // generated Step_1021
    function Step_1021(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_8313
    function Step_8313(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_9409
    function Step_9409(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_8747
    function Step_8747(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_850
    function Step_850(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_765
    function Step_765(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }
}

// Sample Solidity for lexer benchmarking
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

import "./IERC20.sol";

contract BenchVault {
    uint256 public constant MAX_ITEMS = 596;
    string public constant PREFIX = "bench-";

    struct Entry {
        string name;
        uint256 weight;
        bool active;
    }

    mapping(address => Entry) private entries;
    address public owner;

    event EntryAdded(address indexed who, string name, uint256 weight);
    error NotOwner(address caller);

    constructor() {
        owner = msg.sender;
    }

    modifier onlyOwner() {
        if (msg.sender != owner) revert NotOwner(msg.sender);
        _;
    }

    // generated Step_1015
    function Step_1015(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_1880
    function Step_1880(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_5576
    function Step_5576(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_6015
    function Step_6015(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_6176
    function Step_6176(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_7348
    function Step_7348(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }
}

// Sample Solidity for lexer benchmarking
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

import "./IERC20.sol";

contract BenchVault {
    uint256 public constant MAX_ITEMS = 863;
    string public constant PREFIX = "bench-";

    struct Entry {
        string name;
        uint256 weight;
        bool active;
    }

    mapping(address => Entry) private entries;
    address public owner;

    event EntryAdded(address indexed who, string name, uint256 weight);
    error NotOwner(address caller);

    constructor() {
        owner = msg.sender;
    }

    modifier onlyOwner() {
        if (msg.sender != owner) revert NotOwner(msg.sender);
        _;
    }

    // generated Step_8740
    function Step_8740(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_5874
    function Step_5874(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_6337
    function Step_6337(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_6256
    function Step_6256(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_6626
    function Step_6626(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_7369
    function Step_7369(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }
}

// Sample Solidity for lexer benchmarking
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

import "./IERC20.sol";

contract BenchVault {
    uint256 public constant MAX_ITEMS = 524;
    string public constant PREFIX = "bench-";

    struct Entry {
        string name;
        uint256 weight;
        bool active;
    }

    mapping(address => Entry) private entries;
    address public owner;

    event EntryAdded(address indexed who, string name, uint256 weight);
    error NotOwner(address caller);

    constructor() {
        owner = msg.sender;
    }

    modifier onlyOwner() {
        if (msg.sender != owner) revert NotOwner(msg.sender);
        _;
    }

    // generated Step_312
    function Step_312(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_3040
    function Step_3040(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_2338
    function Step_2338(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_6528
    function Step_6528(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_8571
    function Step_8571(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_1847
    function Step_1847(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }
}

// Sample Solidity for lexer benchmarking
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

import "./IERC20.sol";

contract BenchVault {
    uint256 public constant MAX_ITEMS = 761;
    string public constant PREFIX = "bench-";

    struct Entry {
        string name;
        uint256 weight;
        bool active;
    }

    mapping(address => Entry) private entries;
    address public owner;

    event EntryAdded(address indexed who, string name, uint256 weight);
    error NotOwner(address caller);

    constructor() {
        owner = msg.sender;
    }

    modifier onlyOwner() {
        if (msg.sender != owner) revert NotOwner(msg.sender);
        _;
    }

    // generated Step_1977
    function Step_1977(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_7337
    function Step_7337(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_4347
    function Step_4347(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_8958
    function Step_8958(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_6968
    function Step_6968(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_8810
    function Step_8810(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }
}

// Sample Solidity for lexer benchmarking
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

import "./IERC20.sol";

contract BenchVault {
    uint256 public constant MAX_ITEMS = 488;
    string public constant PREFIX = "bench-";

    struct Entry {
        string name;
        uint256 weight;
        bool active;
    }

    mapping(address => Entry) private entries;
    address public owner;

    event EntryAdded(address indexed who, string name, uint256 weight);
    error NotOwner(address caller);

    constructor() {
        owner = msg.sender;
    }

    modifier onlyOwner() {
        if (msg.sender != owner) revert NotOwner(msg.sender);
        _;
    }

    // generated Step_7164
    function Step_7164(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_2747
    function Step_2747(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_7375
    function Step_7375(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_354
    function Step_354(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_7817
    function Step_7817(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_652
    function Step_652(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }
}

// Sample Solidity for lexer benchmarking
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

import "./IERC20.sol";

contract BenchVault {
    uint256 public constant MAX_ITEMS = 689;
    string public constant PREFIX = "bench-";

    struct Entry {
        string name;
        uint256 weight;
        bool active;
    }

    mapping(address => Entry) private entries;
    address public owner;

    event EntryAdded(address indexed who, string name, uint256 weight);
    error NotOwner(address caller);

    constructor() {
        owner = msg.sender;
    }

    modifier onlyOwner() {
        if (msg.sender != owner) revert NotOwner(msg.sender);
        _;
    }

    // generated Step_3215
    function Step_3215(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_3045
    function Step_3045(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_7959
    function Step_7959(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_4916
    function Step_4916(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_6838
    function Step_6838(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_8641
    function Step_8641(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }
}

// Sample Solidity for lexer benchmarking
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

import "./IERC20.sol";

contract BenchVault {
    uint256 public constant MAX_ITEMS = 480;
    string public constant PREFIX = "bench-";

    struct Entry {
        string name;
        uint256 weight;
        bool active;
    }

    mapping(address => Entry) private entries;
    address public owner;

    event EntryAdded(address indexed who, string name, uint256 weight);
    error NotOwner(address caller);

    constructor() {
        owner = msg.sender;
    }

    modifier onlyOwner() {
        if (msg.sender != owner) revert NotOwner(msg.sender);
        _;
    }

    // generated Step_520
    function Step_520(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_979
    function Step_979(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_4419
    function Step_4419(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_5696
    function Step_5696(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_6603
    function Step_6603(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_7583
    function Step_7583(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }
}

// Sample Solidity for lexer benchmarking
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

import "./IERC20.sol";

contract BenchVault {
    uint256 public constant MAX_ITEMS = 611;
    string public constant PREFIX = "bench-";

    struct Entry {
        string name;
        uint256 weight;
        bool active;
    }

    mapping(address => Entry) private entries;
    address public owner;

    event EntryAdded(address indexed who, string name, uint256 weight);
    error NotOwner(address caller);

    constructor() {
        owner = msg.sender;
    }

    modifier onlyOwner() {
        if (msg.sender != owner) revert NotOwner(msg.sender);
        _;
    }

    // generated Step_8895
    function Step_8895(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_4978
    function Step_4978(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_7267
    function Step_7267(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_5497
    function Step_5497(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_4080
    function Step_4080(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_4181
    function Step_4181(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }
}

// Sample Solidity for lexer benchmarking
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

import "./IERC20.sol";

contract BenchVault {
    uint256 public constant MAX_ITEMS = 571;
    string public constant PREFIX = "bench-";

    struct Entry {
        string name;
        uint256 weight;
        bool active;
    }

    mapping(address => Entry) private entries;
    address public owner;

    event EntryAdded(address indexed who, string name, uint256 weight);
    error NotOwner(address caller);

    constructor() {
        owner = msg.sender;
    }

    modifier onlyOwner() {
        if (msg.sender != owner) revert NotOwner(msg.sender);
        _;
    }

    // generated Step_764
    function Step_764(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_9743
    function Step_9743(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_2832
    function Step_2832(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_2309
    function Step_2309(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_125
    function Step_125(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_6144
    function Step_6144(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }
}

// Sample Solidity for lexer benchmarking
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

import "./IERC20.sol";

contract BenchVault {
    uint256 public constant MAX_ITEMS = 911;
    string public constant PREFIX = "bench-";

    struct Entry {
        string name;
        uint256 weight;
        bool active;
    }

    mapping(address => Entry) private entries;
    address public owner;

    event EntryAdded(address indexed who, string name, uint256 weight);
    error NotOwner(address caller);

    constructor() {
        owner = msg.sender;
    }

    modifier onlyOwner() {
        if (msg.sender != owner) revert NotOwner(msg.sender);
        _;
    }

    // generated Step_45
    function Step_45(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_1566
    function Step_1566(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_3205
    function Step_3205(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_7185
    function Step_7185(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_9503
    function Step_9503(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_2614
    function Step_2614(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }
}

// Sample Solidity for lexer benchmarking
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

import "./IERC20.sol";

contract BenchVault {
    uint256 public constant MAX_ITEMS = 415;
    string public constant PREFIX = "bench-";

    struct Entry {
        string name;
        uint256 weight;
        bool active;
    }

    mapping(address => Entry) private entries;
    address public owner;

    event EntryAdded(address indexed who, string name, uint256 weight);
    error NotOwner(address caller);

    constructor() {
        owner = msg.sender;
    }

    modifier onlyOwner() {
        if (msg.sender != owner) revert NotOwner(msg.sender);
        _;
    }

    // generated Step_9323
    function Step_9323(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_3313
    function Step_3313(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_3402
    function Step_3402(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_785
    function Step_785(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_9822
    function Step_9822(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }

    // generated Step_8403
    function Step_8403(uint256 seed) external view onlyOwner returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i < MAX_ITEMS; i++) {
            if (i %% 2 == 0) {
                total += i * seed;
            } else {
                total -= i %% 7;
            }
        }
        return total;
    }
}

