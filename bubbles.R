# bubbles stores the radius and color of bubbles.
# dropped.bubbles stores the x and y positions of already dropped bubbles.

rotate.random <- function(XY) {
    # Rotation matrix R.
    theta <- runif(1)*2*pi # Random rotation angle theta.
    R <- matrix(c(cos(theta), -sin(theta),
                  sin(theta), cos(theta)
                  ),
                byrow=TRUE,
                ncol=2,
                nrow=2
                )
    return(XY %*% R)
}

spiralize.coords <- function(distance) {
    # Convert a distance along the spiral line into x,y coordinates.
    position.x <- distance * cos(distance)
    position.y <- distance * sin(distance)

    return(c(position.x, position.y))
}

overlap <- function(distance, bubble.radius, el.bagot, radii) {
    # Step 1. Compute delta, the vector of distances between the centers
    # of the bubbles and the bubble to drop.
    delta <- rowSums(sweep(el.bagot,
                           STATS=distance,
                           MARGIN=2)^2)
    # Step 2. Check whether any bubble overlap.
    return (any((bubble.radius + radii) > sqrt(delta)))
}

print.bubbles <- function(dropped.bubbles, bubbles, flavors, color) {
    symbols(dropped.bubbles[which(bubbles[,2] + 1 == color),1],
            dropped.bubbles[which(bubbles[,2] + 1 == color),2],
            circles=bubbles[which(bubbles[,2] + 1 == color),1],
            inches=FALSE,
            axes=FALSE,
            ann=FALSE,
            fg=flavors[color],
            bg=flavors[color],
            xlim=c(-60, 90),
            ylim=c(-60, 60)
            )
    par(new=TRUE)
}

all.bubbles <- read.table('/home/gfilion/data/domains.txt',
                          header=TRUE, as.is=TRUE)
unsorted.bubbles <- all.bubbles[sample(nrow(all.bubbles), 1000),]
square.bubbles <- unsorted.bubbles[order(-unsorted.bubbles$size),]
bubbles <- cbind(sqrt(square.bubbles$size/pi), square.bubbles$type)
epsilon <- 0.01 # Arbitrary value for the step size along the spiral.

# XY positions of dropped bubbles. First bubble is dropped at center.
dropped.bubbles <- matrix(c(0,0), ncol=2)

for (i in 2:nrow(bubbles)) {
    bubble <- bubbles[i,]
    dropped.bubbles <- rotate.random(dropped.bubbles)
    el.bagot <- dropped.bubbles[,,drop=FALSE]
    radii <- bubbles[,1]

    distance <- 0 # Distance from center, along the line of the spiral.
    dropped <- FALSE
    while (!dropped) {
        xy.distance <- spiralize.coords(distance)
        if (!overlap(xy.distance, bubble[1][1], el.bagot, radii)) {
            dropped <- TRUE # Drop the bubble!
            dropped.bubbles <- rbind(dropped.bubbles,
                                     c(xy.distance)
                                     )
        } else {
            distance <- distance + epsilon*10 # Padding is proportional to step.
        }
    }
}

dropped.bubbles <- rotate.random(dropped.bubbles)

#flavors <- c("blue", "gold2", "red", "seagreen3", "black", "some_grey")
flavors <- c("#0000FFAA", "#EEC800AA", "#FF0000AA",
             "#43CD80AA", "#000000"  , "#858585AA")
for (color in 1:length(flavors)) {
    print.bubbles(dropped.bubbles, bubbles, flavors, color)
}

# Add legend in grey.
biggest.radius <- max(bubbles[,1])
biggest.area <- pi*biggest.radius^2
areas <- biggest.area * (1/4)^(0:4)
legend.radii <- sqrt(areas/pi)
# Show in the legend the smallest value of the graph (1 window).
legend.radii[length(legend.radii)] <- sqrt(1/pi)

legend.radius.color <- cbind((legend.radii), rep(5,length(legend.radii)))
legend.xy <- cbind(rep(65,length(legend.radii)), 4.5*legend.radii)

par(new=TRUE)
print.bubbles(legend.xy, legend.radius.color, flavors, 6)
text(71+legend.radii, 4.5*legend.radii,
     labels=paste(round(3*pi*legend.radii^2), "kbp"), cex=0.8,
     vfont=c("sans serif","bold"))
