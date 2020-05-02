fmainPos <- 
function(pu, main.pos)
{
  d <- abs(abs(pu[4]) - abs(pu[3])) * 0.05
  mp <- match.arg(main.pos, c("topright", "topleft", 
                              "bottomright", "bottomleft"))
  cx <- switch(mp, topright = pu[2], topleft = pu[1],
                   bottomright = pu[2], bottomleft = pu[1])
  cy <- switch(mp, topright = pu[4] - d, topleft = pu[4] - d,
                   bottomright = pu[3] + d, bottomleft = pu[3] + d)
  pos <- switch(mp, topright = 2, topleft = 4,
                    bottomright = 2, bottomleft = 4)
  ans <- list(cx = cx, cy = cy, pos = pos)
  ans
}