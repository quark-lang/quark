/*//////////////////////////////////////
               Quark lang
                Comments
//////////////////////////////////////*/

export default class Comments {

    private static          correct_text  : Array<string> = []
    private static readonly coefficient   : number        = 4
    private static readonly trails_start  : string        = '/*/' 
    private static          trails_length : any           = []
    private static          space_length  : Array<number> = []

    /**
     * @private
     * @static
     * 
     * @name        set_trails_length
     * @description Set trails length
     * @author      NessMC
     */

    private static set_trails_length () : void {
        for (const line of this.correct_text) {
            if ((((line.length * this.coefficient) - line.length) / 2) % 1 === 0) {
                this.trails_length.push(line.length * this.coefficient)
            } else {
                this.trails_length.push(line.length * this.coefficient + 1)
            }
        }

        this.trails_length = Math.max(...this.trails_length)
    }

    /**
     * @private
     * @static
     * 
     * @name        set_space_length
     * @description Set space length
     * @author      NessMC
     */

    private static set_space_length () : void {

        for (const line of this.correct_text) {
            if (((this.trails_length - line.length) / 2) % 1 === 0) {
                this.space_length.push((this.trails_length - line.length) / 2)
            } else {
                this.space_length.push((this.trails_length - line.length + 1) / 2)
            }
        }

    }

    /**
     * @public
     * @static
     * 
     * @name        generate
     * @description Generate comment
     * @author      NessMC
     */

    public static generate (...text : Array<string>) : string {

        this.correct_text   = text.filter(x => x.length % 2 === text[0].length % 2 ? x.length : '')

        this.set_trails_length()
        this.set_space_length()

        const trails_content : Array<string>        = [...this.trails_start].concat(new Array(this.trails_length - 3)
                                                                            .fill('/')),
              space_content  : Array<Array<string>> = this.correct_text.map((x, index) => new Array(this.space_length[index]).fill(' ')),
              text_content   : Array<string>        = this.correct_text.map((x, index) => space_content[index].join('')
                                                                                       .concat(x)
                                                                                       .concat(space_content[index]
                                                                                       .join(''))),
              result         : string               = trails_content.join('') + '\n' + text_content.join('\n') + '\n' + trails_content.reverse().join('')
                                                     
        return result

    }

}
