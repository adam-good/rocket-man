import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
from mpl_toolkits.mplot3d import Axes3D

# Read the CSV file
df = pd.read_csv('output/test.csv')

# Extract data
timestamps = df['time']
x = df['x']
y = df['y']
z = df['z']

target_x = [1]
target_y = [1]
target_z = [1]

# Create a 3D plot
fig = plt.figure(figsize=(10, 8))
ax = fig.add_subplot(111, projection='3d')

# Initialize scatter plot
scatter = ax.scatter([], [], [], c='b', marker='o', label="projectile")
target = ax.scatter([], [], [], c='r', marker='x', s=100, label="target")

# Set labels and title
ax.set_xlabel('X')
ax.set_ylabel('Y')
ax.set_zlabel('Z')
ax.set_title('Projectile Guidance Simulation')

ax.legend(loc="upper right")

# Initialize the plot with empty data
def init():
    scatter._offsets3d = ([], [], [])
    target._offset3d = ([], [], [])
    return scatter, target

# Update function for animation
def update(frame):
    # Update data up to the current frame
    scatter._offsets3d = (x[:frame+1], y[:frame+1], z[:frame+1])
    target._offsets3d = (target_x, target_y, target_z)
    ax.set_xlim(0, max(x+target_x))
    ax.set_ylim(0, max(y+target_y))
    ax.set_zlim(0, max(z+target_z))
    return scatter, target

# Create animation
ani = FuncAnimation(
    fig,
    update,
    frames=len(df),
    init_func=init,
    blit=False,
    interval=50,  # Delay between frames in milliseconds
    repeat=False
)

# Save the animation
ani.save('output/animation.mp4', writer='ffmpeg', fps=30, dpi=200)

# Show the plot
plt.show()