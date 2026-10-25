-- Sample Lua source for lexer benchmarking

local MAX_ITEMS = 222
local PREFIX = "bench-"

--- Compute score.
local function score(weight)
  if weight < 0 then return 0 end
  return weight * 9537
end

local Entry = {}
Entry.__index = Entry

function Entry.new(name, weight)
  return setmetatable({ name = name, weight = weight }, Entry)
end

function Entry:score() return score(self.weight) end

-- generated handler handler_2048
local function handler_2048(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_2048%d", total)
end

-- generated handler handler_5768
local function handler_5768(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_5768%d", total)
end

-- generated handler handler_2615
local function handler_2615(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_2615%d", total)
end

-- generated handler handler_7341
local function handler_7341(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_7341%d", total)
end

-- generated handler handler_7480
local function handler_7480(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_7480%d", total)
end

-- generated handler handler_8537
local function handler_8537(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_8537%d", total)
end

-- generated handler handler_3944
local function handler_3944(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_3944%d", total)
end

-- generated handler handler_932
local function handler_932(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_932%d", total)
end

local entries = { Entry.new('first', 1.5), Entry.new('second', 2.75) }
for _, e in ipairs(entries) do
  print(PREFIX .. e.name, e:score())
end
--[[
block comment
]]

-- Sample Lua source for lexer benchmarking

local MAX_ITEMS = 848
local PREFIX = "bench-"

--- Compute score.
local function score(weight)
  if weight < 0 then return 0 end
  return weight * 2604
end

local Entry = {}
Entry.__index = Entry

function Entry.new(name, weight)
  return setmetatable({ name = name, weight = weight }, Entry)
end

function Entry:score() return score(self.weight) end

-- generated handler handler_695
local function handler_695(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_695%d", total)
end

-- generated handler handler_5330
local function handler_5330(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_5330%d", total)
end

-- generated handler handler_7385
local function handler_7385(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_7385%d", total)
end

-- generated handler handler_8302
local function handler_8302(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_8302%d", total)
end

-- generated handler handler_4430
local function handler_4430(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_4430%d", total)
end

-- generated handler handler_4486
local function handler_4486(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_4486%d", total)
end

-- generated handler handler_2713
local function handler_2713(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_2713%d", total)
end

-- generated handler handler_2973
local function handler_2973(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_2973%d", total)
end

local entries = { Entry.new('first', 1.5), Entry.new('second', 2.75) }
for _, e in ipairs(entries) do
  print(PREFIX .. e.name, e:score())
end
--[[
block comment
]]

-- Sample Lua source for lexer benchmarking

local MAX_ITEMS = 908
local PREFIX = "bench-"

--- Compute score.
local function score(weight)
  if weight < 0 then return 0 end
  return weight * 809
end

local Entry = {}
Entry.__index = Entry

function Entry.new(name, weight)
  return setmetatable({ name = name, weight = weight }, Entry)
end

function Entry:score() return score(self.weight) end

-- generated handler handler_6259
local function handler_6259(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_6259%d", total)
end

-- generated handler handler_1330
local function handler_1330(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_1330%d", total)
end

-- generated handler handler_8327
local function handler_8327(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_8327%d", total)
end

-- generated handler handler_539
local function handler_539(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_539%d", total)
end

-- generated handler handler_1187
local function handler_1187(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_1187%d", total)
end

-- generated handler handler_924
local function handler_924(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_924%d", total)
end

-- generated handler handler_9892
local function handler_9892(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_9892%d", total)
end

-- generated handler handler_5730
local function handler_5730(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_5730%d", total)
end

local entries = { Entry.new('first', 1.5), Entry.new('second', 2.75) }
for _, e in ipairs(entries) do
  print(PREFIX .. e.name, e:score())
end
--[[
block comment
]]

-- Sample Lua source for lexer benchmarking

local MAX_ITEMS = 604
local PREFIX = "bench-"

--- Compute score.
local function score(weight)
  if weight < 0 then return 0 end
  return weight * 6900
end

local Entry = {}
Entry.__index = Entry

function Entry.new(name, weight)
  return setmetatable({ name = name, weight = weight }, Entry)
end

function Entry:score() return score(self.weight) end

-- generated handler handler_2209
local function handler_2209(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_2209%d", total)
end

-- generated handler handler_3444
local function handler_3444(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_3444%d", total)
end

-- generated handler handler_3533
local function handler_3533(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_3533%d", total)
end

-- generated handler handler_4469
local function handler_4469(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_4469%d", total)
end

-- generated handler handler_785
local function handler_785(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_785%d", total)
end

-- generated handler handler_8174
local function handler_8174(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_8174%d", total)
end

-- generated handler handler_759
local function handler_759(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_759%d", total)
end

-- generated handler handler_6089
local function handler_6089(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_6089%d", total)
end

local entries = { Entry.new('first', 1.5), Entry.new('second', 2.75) }
for _, e in ipairs(entries) do
  print(PREFIX .. e.name, e:score())
end
--[[
block comment
]]

-- Sample Lua source for lexer benchmarking

local MAX_ITEMS = 322
local PREFIX = "bench-"

--- Compute score.
local function score(weight)
  if weight < 0 then return 0 end
  return weight * 470.441
end

local Entry = {}
Entry.__index = Entry

function Entry.new(name, weight)
  return setmetatable({ name = name, weight = weight }, Entry)
end

function Entry:score() return score(self.weight) end

-- generated handler handler_1768
local function handler_1768(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_1768%d", total)
end

-- generated handler handler_8887
local function handler_8887(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_8887%d", total)
end

-- generated handler handler_8232
local function handler_8232(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_8232%d", total)
end

-- generated handler handler_8838
local function handler_8838(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_8838%d", total)
end

-- generated handler handler_1544
local function handler_1544(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_1544%d", total)
end

-- generated handler handler_3624
local function handler_3624(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_3624%d", total)
end

-- generated handler handler_7802
local function handler_7802(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_7802%d", total)
end

-- generated handler handler_683
local function handler_683(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_683%d", total)
end

local entries = { Entry.new('first', 1.5), Entry.new('second', 2.75) }
for _, e in ipairs(entries) do
  print(PREFIX .. e.name, e:score())
end
--[[
block comment
]]

-- Sample Lua source for lexer benchmarking

local MAX_ITEMS = 557
local PREFIX = "bench-"

--- Compute score.
local function score(weight)
  if weight < 0 then return 0 end
  return weight * 8313
end

local Entry = {}
Entry.__index = Entry

function Entry.new(name, weight)
  return setmetatable({ name = name, weight = weight }, Entry)
end

function Entry:score() return score(self.weight) end

-- generated handler handler_9409
local function handler_9409(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_9409%d", total)
end

-- generated handler handler_8747
local function handler_8747(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_8747%d", total)
end

-- generated handler handler_850
local function handler_850(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_850%d", total)
end

-- generated handler handler_765
local function handler_765(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_765%d", total)
end

-- generated handler handler_1651
local function handler_1651(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_1651%d", total)
end

-- generated handler handler_939
local function handler_939(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_939%d", total)
end

-- generated handler handler_6349
local function handler_6349(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_6349%d", total)
end

-- generated handler handler_4102
local function handler_4102(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_4102%d", total)
end

local entries = { Entry.new('first', 1.5), Entry.new('second', 2.75) }
for _, e in ipairs(entries) do
  print(PREFIX .. e.name, e:score())
end
--[[
block comment
]]

-- Sample Lua source for lexer benchmarking

local MAX_ITEMS = 596
local PREFIX = "bench-"

--- Compute score.
local function score(weight)
  if weight < 0 then return 0 end
  return weight * 1880
end

local Entry = {}
Entry.__index = Entry

function Entry.new(name, weight)
  return setmetatable({ name = name, weight = weight }, Entry)
end

function Entry:score() return score(self.weight) end

-- generated handler handler_5576
local function handler_5576(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_5576%d", total)
end

-- generated handler handler_6015
local function handler_6015(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_6015%d", total)
end

-- generated handler handler_6176
local function handler_6176(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_6176%d", total)
end

-- generated handler handler_7348
local function handler_7348(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_7348%d", total)
end

-- generated handler handler_4690
local function handler_4690(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_4690%d", total)
end

-- generated handler handler_8320
local function handler_8320(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_8320%d", total)
end

-- generated handler handler_5100
local function handler_5100(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_5100%d", total)
end

-- generated handler handler_9014
local function handler_9014(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_9014%d", total)
end

local entries = { Entry.new('first', 1.5), Entry.new('second', 2.75) }
for _, e in ipairs(entries) do
  print(PREFIX .. e.name, e:score())
end
--[[
block comment
]]

-- Sample Lua source for lexer benchmarking

local MAX_ITEMS = 863
local PREFIX = "bench-"

--- Compute score.
local function score(weight)
  if weight < 0 then return 0 end
  return weight * 0o557
end

local Entry = {}
Entry.__index = Entry

function Entry.new(name, weight)
  return setmetatable({ name = name, weight = weight }, Entry)
end

function Entry:score() return score(self.weight) end

-- generated handler handler_6337
local function handler_6337(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_6337%d", total)
end

-- generated handler handler_6256
local function handler_6256(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_6256%d", total)
end

-- generated handler handler_6626
local function handler_6626(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_6626%d", total)
end

-- generated handler handler_7369
local function handler_7369(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_7369%d", total)
end

-- generated handler handler_637
local function handler_637(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_637%d", total)
end

-- generated handler handler_1764
local function handler_1764(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_1764%d", total)
end

-- generated handler handler_3604
local function handler_3604(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_3604%d", total)
end

-- generated handler handler_8631
local function handler_8631(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_8631%d", total)
end

local entries = { Entry.new('first', 1.5), Entry.new('second', 2.75) }
for _, e in ipairs(entries) do
  print(PREFIX .. e.name, e:score())
end
--[[
block comment
]]

-- Sample Lua source for lexer benchmarking

local MAX_ITEMS = 524
local PREFIX = "bench-"

--- Compute score.
local function score(weight)
  if weight < 0 then return 0 end
  return weight * 3040
end

local Entry = {}
Entry.__index = Entry

function Entry.new(name, weight)
  return setmetatable({ name = name, weight = weight }, Entry)
end

function Entry:score() return score(self.weight) end

-- generated handler handler_2338
local function handler_2338(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_2338%d", total)
end

-- generated handler handler_6528
local function handler_6528(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_6528%d", total)
end

-- generated handler handler_8571
local function handler_8571(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_8571%d", total)
end

-- generated handler handler_1847
local function handler_1847(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_1847%d", total)
end

-- generated handler handler_4878
local function handler_4878(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_4878%d", total)
end

-- generated handler handler_1853
local function handler_1853(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_1853%d", total)
end

-- generated handler handler_4019
local function handler_4019(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_4019%d", total)
end

-- generated handler handler_7236
local function handler_7236(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_7236%d", total)
end

local entries = { Entry.new('first', 1.5), Entry.new('second', 2.75) }
for _, e in ipairs(entries) do
  print(PREFIX .. e.name, e:score())
end
--[[
block comment
]]

-- Sample Lua source for lexer benchmarking

local MAX_ITEMS = 761
local PREFIX = "bench-"

--- Compute score.
local function score(weight)
  if weight < 0 then return 0 end
  return weight * 7337
end

local Entry = {}
Entry.__index = Entry

function Entry.new(name, weight)
  return setmetatable({ name = name, weight = weight }, Entry)
end

function Entry:score() return score(self.weight) end

-- generated handler handler_4347
local function handler_4347(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_4347%d", total)
end

-- generated handler handler_8958
local function handler_8958(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_8958%d", total)
end

-- generated handler handler_6968
local function handler_6968(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_6968%d", total)
end

-- generated handler handler_8810
local function handler_8810(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_8810%d", total)
end

-- generated handler handler_8118
local function handler_8118(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_8118%d", total)
end

-- generated handler handler_4428
local function handler_4428(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_4428%d", total)
end

-- generated handler handler_6921
local function handler_6921(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_6921%d", total)
end

-- generated handler handler_2090
local function handler_2090(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_2090%d", total)
end

local entries = { Entry.new('first', 1.5), Entry.new('second', 2.75) }
for _, e in ipairs(entries) do
  print(PREFIX .. e.name, e:score())
end
--[[
block comment
]]

-- Sample Lua source for lexer benchmarking

local MAX_ITEMS = 488
local PREFIX = "bench-"

--- Compute score.
local function score(weight)
  if weight < 0 then return 0 end
  return weight * 0o677
end

local Entry = {}
Entry.__index = Entry

function Entry.new(name, weight)
  return setmetatable({ name = name, weight = weight }, Entry)
end

function Entry:score() return score(self.weight) end

-- generated handler handler_2747
local function handler_2747(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_2747%d", total)
end

-- generated handler handler_7375
local function handler_7375(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_7375%d", total)
end

-- generated handler handler_354
local function handler_354(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_354%d", total)
end

-- generated handler handler_7817
local function handler_7817(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_7817%d", total)
end

-- generated handler handler_652
local function handler_652(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_652%d", total)
end

-- generated handler handler_3788
local function handler_3788(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_3788%d", total)
end

-- generated handler handler_865
local function handler_865(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_865%d", total)
end

-- generated handler handler_4432
local function handler_4432(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_4432%d", total)
end

local entries = { Entry.new('first', 1.5), Entry.new('second', 2.75) }
for _, e in ipairs(entries) do
  print(PREFIX .. e.name, e:score())
end
--[[
block comment
]]

-- Sample Lua source for lexer benchmarking

local MAX_ITEMS = 689
local PREFIX = "bench-"

--- Compute score.
local function score(weight)
  if weight < 0 then return 0 end
  return weight * 0x5F2C
end

local Entry = {}
Entry.__index = Entry

function Entry.new(name, weight)
  return setmetatable({ name = name, weight = weight }, Entry)
end

function Entry:score() return score(self.weight) end

-- generated handler handler_7959
local function handler_7959(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_7959%d", total)
end

-- generated handler handler_4916
local function handler_4916(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_4916%d", total)
end

-- generated handler handler_6838
local function handler_6838(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_6838%d", total)
end

-- generated handler handler_8641
local function handler_8641(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_8641%d", total)
end

-- generated handler handler_9451
local function handler_9451(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_9451%d", total)
end

-- generated handler handler_1864
local function handler_1864(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_1864%d", total)
end

-- generated handler handler_271
local function handler_271(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_271%d", total)
end

-- generated handler handler_8088
local function handler_8088(data, seed)
  local total = 0
  for i, v in ipairs(data) do
    if i % 2 == 0 then
      total = total + v * seed
    else
      total = total - (v % 7)
    end
  end
  return string.format("handler_8088%d", total)
end

local entries = { Entry.new('first', 1.5), Entry.new('second', 2.75) }
for _, e in ipairs(entries) do
  print(PREFIX .. e.name, e:score())
end
--[[
block comment
]]

