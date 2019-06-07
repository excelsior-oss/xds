
local filename = ...

local locations = {}

for l in io.lines (filename) do
  local loc, amount = l:match "^([^%s]*) (%d*)$"
  
  if loc then 
    local data = locations[loc] or { size = 0, count = 0 }
    data.size, data.count = data.size + tonumber(amount), data.count + 1
    locations[loc] = data
  end
end

local locs = {}

for k, v in pairs(locations) do
  table.insert(locs, {k, v})
end

local Kb = 1024
local Mb = 1024 * Kb
local Gb = 1024 * Mb

local function s2h(s)
  if s > Gb then
    return ("%.1f G"):format(s/Gb)
  elseif s > Mb then
    return ("%.1f M"):format(s/Mb)
  elseif s > Kb then
    return ("%.1f K"):format(s/Kb)  
  end
  return tostring(s)
end

table.sort(locs, function(a, b) return a[2].size > b[2].size end)

print(string.format("%-20s | %-10s | %-11s ", "ALLOCATION SITE", "TOTAL SIZE", "BLOCK COUNT"))
print(("-"):rep(20 + 3 + 10 + 3 + 11 + 3))
for i = 1, #locs do
  local loc = locs[i]

  print(string.format("%-20s | %-10s | %-10d", loc[1]:match "\\([^\\]*)$" or loc[1], s2h (loc[2].size), loc[2].count))
end
