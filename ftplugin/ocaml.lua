-- This plugin provides a :SayHello command to say hello.

local path_to_exe = vim.fn.expand("<sfile>:h") .. "/bin/main.exe"

vim.api.nvim_create_user_command(
  "AllocScan",
  function(args)
    local job = vim.fn.jobstart({ path_to_exe }, { rpc = true })
    vim.rpcrequest(job, "allocScan")
  end,
  { bar = true, nargs = 0 })

vim.api.nvim_create_user_command(
  "AllocScanFile",
  function(args)
    local job = vim.fn.jobstart({ path_to_exe }, { rpc = true })
    vim.rpcrequest(job, "allocScanFile", args.args)
  end,
  { bar = true, nargs = 1, complete = "user" })

vim.api.nvim_create_user_command(
  "AllocClear",
  function(args)
    local job = vim.fn.jobstart({ path_to_exe }, { rpc = true })
    vim.rpcrequest(job, "allocClear")
  end,
  { bar = true, nargs = 0 })

