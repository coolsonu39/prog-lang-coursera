# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  All_My_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
               rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
               [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
               [[0, 0], [0, -1], [0, 1], [0, 2]]],
               rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
               rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
               rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
               rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # Z
               rotations([[0, 0], [-1, 0], [1, 0], [0, -1], [-1, -1]]), # new1
               [[[0, 0], [-1, 0], [1, 0], [2, 0], [-2, 0]], # new2
               [[0, 0], [0, -1], [0, 1], [0, 2], [0, -2]]],
               rotations([[0, 0], [0, -1], [1, 0]])] # new3
  
  # your enhancements here
  def initialize (point_array, board)
  	super
  end

  def self.next_piece (board, cheat)
  	if cheat
  		MyPiece.new([[[0, 0]]], board)
  	else
    	MyPiece.new(All_My_Pieces.sample, board)
    end
  end
end

class MyBoard < Board
  # your enhancements here
  def initialize (game)
  	super
  	@current_block = MyPiece.next_piece(self, false)
  	@cheat = false
  end

  def next_piece
  	if @cheat
  		@current_block = MyPiece.next_piece(self, true)
	else
		@current_block = MyPiece.next_piece(self, false)
	end
    @current_pos = nil
    @cheat = false
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..(locations.length-1)).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  def cheat
  	if @cheat==false and @score >= 100
  		@cheat = true
  		@score -= 100
  	end
  end
end

class MyTetris < Tetris
  # your enhancements here
  def initialize
  	super
  end

  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
  	super
  	@root.bind('u', proc do
  		@board.rotate_counter_clockwise
  		@board.rotate_counter_clockwise
  	end)

  	@root.bind('c', proc do
  		@board.cheat
  	end)
  end
end